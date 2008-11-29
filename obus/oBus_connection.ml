(*
 * oBus_connection.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module Log = Log.Make(struct let section = "connection" end)

open Printf
open OBus_message
open OBus_internals
open OBus_type
open OBus_value
open Lwt

type guid = OBus_address.guid
type name = string

exception Connection_closed
exception Connection_lost
exception Transport_error of exn

type t = connection

type filter = OBus_internals.filter
type filter_id = filter MSet.node * filter MSet.t

(* Mapping from server guid to connection. *)
module Guid_map = My_map(struct type t = guid end)
let guid_connection_map = ref Guid_map.empty

let remove_connection_of_guid_map = function
  | { guid = Some guid } -> guid_connection_map := Guid_map.remove guid !guid_connection_map
  | { guid = None } -> ()

let apply_filters typ message filters =
  try
    MSet.filter filters message
  with
      exn ->
        Log.failure exn "%s filters failed with" typ;
        None

(***** Error handling *****)

(* Put the connection in a "crashed" state. This means that all
   subsequent call using the connection will fail. *)
let set_crash connection exn = match connection.crashed with
  | Some exn -> exn
  | None ->
      connection.crashed <- Some exn;
      remove_connection_of_guid_map connection;

      let { reply_waiters = reply_waiters;
            exported_objects = exported_objects } = connection in

      MSet.clear connection.signal_receivers;
      MSet.clear connection.incoming_filters;
      MSet.clear connection.outgoing_filters;
      connection.reply_waiters <- Serial_map.empty;
      connection.exported_objects <- Object_map.empty;

      (* This make the dispatcher to exit if it is waiting on
         [get_message] *)
      wakeup_exn connection.abort exn;
      (match connection.down with
         | Some w -> wakeup_exn w exn
         | None -> ());

      (* Shutdown the transport *)
      if !(connection.shutdown_transport_on_close) then
        (try
           OBus_lowlevel.shutdown connection.transport
         with
             exn ->
               Log.failure exn "failed to abort/shutdown the transport");

      (* Wakeup all reply handlers so they will not wait forever *)
      Serial_map.iter (fun _ w -> wakeup_exn w exn) reply_waiters;

      (* Remove all objects *)
      Object_map.iter begin fun p obj ->
        try
          obj#obus_connection_closed connection
        with
            exn ->
              (* This may happen if the programmer has overridden the
                 method *)
              Log.failure exn "obus_connection_closed on object with path %S failed with"
                (OBus_path.to_string p)
      end exported_objects;

      exn

let close = with_running (fun connection -> ignore (set_crash connection Connection_closed))

(* Get the error message of an error *)
let get_error msg = match msg.body with
  | Basic String x :: _ -> x
  | _ -> ""

(***** Sending messages *****)

let send_message_backend reply_waiter return_thread connection message =
  lwt_with_running begin fun connection ->
    let outgoing = connection.outgoing in
    let w = wait () in
    connection.outgoing <- w;
    outgoing >>= fun serial ->
      match apply_filters "outgoing" { (message :> any) with serial = serial } connection.outgoing_filters with
        | None ->
            Log.debug "outgoing message dropped by filters";
            wakeup w serial;
            fail (Failure "message dropped by filters")

        | Some message ->
            begin match reply_waiter with
              | Some w ->
                  connection.reply_waiters <- Serial_map.add serial w connection.reply_waiters
              | None -> ()
            end;

            if !(OBus_info.dump) then
              Format.eprintf "-----@\n@[<hv 2>sending message:@\n%a@]@."
                OBus_message.print message;

            try_bind
              (fun _ -> OBus_lowlevel.send connection.transport message)
              (fun _ ->
                 (* Everything went OK, continue with a new serial *)
                 wakeup w (Int32.succ serial);
                 return_thread)
              (function
                 | OBus_lowlevel.Data_error _ as exn ->
                     (* The message can not be marshaled for some
                        reason. This is not a fatal error. *)
                     wakeup w serial;
                     fail exn

                 | exn ->
                     (* All other errors are considered as fatal. They
                        are fatal because it is possible that a
                        message has been partially sent on the
                        connection, so the message stream is broken *)
                     let exn = set_crash connection (Transport_error exn) in
                     wakeup_exn w exn;
                     fail exn)
  end connection


let send_message connection message =
  send_message_backend None (return ()) connection message

let send_message_with_reply connection message =
  let w = wait () in
  send_message_backend (Some w) w connection message

(***** Helpers *****)

exception Context of connection * OBus_message.any
let mk_context connection msg = Context(connection, (msg :> OBus_message.any))

let tt = wrap_sequence_ctx tunit
  (fun context () -> match context with
     | Context(connection, msg) -> connection
     | _ -> raise Cast_failure)
  (fun _ -> ())

let method_call' connection ?flags ?sender ?destination ~path ?interface ~member body ty_reply =
  send_message_with_reply connection (method_call ?flags ?sender ?destination ~path ?interface ~member body)
  >>= fun msg ->
    try
      return (cast_sequence ~context:(mk_context connection msg) ty_reply msg.body)
    with
      | Cast_failure ->
          (* If not, check why the cast fail *)
          let expected_sig = type_sequence ty_reply
          and got_sig = type_of_sequence msg.body in
          if expected_sig = got_sig
          then
            (* If the signature match, this means that the user
               defined a combinator raising a Cast_failure *)
            fail Cast_failure
          else
            (* In other case this means that the expected signature is
               wrong *)
            fail
              (Failure (sprintf "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                          member (match interface with Some i -> i | None -> "")
                          (string_of_signature expected_sig)
                          (string_of_signature got_sig)))

let method_call_no_reply connection ?(flags=default_flags) ?sender ?destination ~path ?interface ~member ty =
  make_func ty begin fun body ->
    send_message connection (method_call ~flags:{ flags with no_reply_expected = true }
                               ?sender ?destination ~path ?interface ~member body)
  end

let dmethod_call connection ?flags ?sender ?destination ~path ?interface ~member body =
  send_message_with_reply connection
    (method_call ?flags ?sender ?destination ~path ?interface ~member body)
  >>= fun { body = x } -> return x

let dmethod_call_no_reply connection ?(flags=default_flags) ?sender ?destination ~path ?interface ~member body =
  send_message connection
    (method_call ~flags:{ flags with no_reply_expected = true }
       ?sender ?destination ~path ?interface ~member body)

let method_call connection ?flags ?sender ?destination ~path ?interface ~member ty =
  make_func ty begin fun body ->
    method_call' connection ?flags ?sender ?destination ~path ?interface ~member body (func_reply ty)
  end

let emit_signal connection ?flags ?sender ?destination ~path ~interface ~member ty x =
  send_message connection (signal ?flags ?sender ?destination ~path ~interface ~member (make_sequence ty x))

let demit_signal connection ?flags ?sender ?destination ~path ~interface ~member body =
  send_message connection (signal ?flags ?sender ?destination ~path ~interface ~member body)

let dsend_reply connection { sender = sender; serial = serial } body =
  send_message connection { destination = sender;
                            sender = None;
                            flags = { no_reply_expected = true; no_auto_start = true };
                            serial = 0l;
                            typ = `Method_return(serial);
                            body = body }

let send_reply connection mc typ v =
  dsend_reply connection mc (make_sequence typ v)

let send_error connection { sender = sender; serial = serial } name msg =
  send_message connection { destination = sender;
                            sender = None;
                            flags = { no_reply_expected = true; no_auto_start = true };
                            serial = 0l;
                            typ = `Error(serial, name);
                            body = [vbasic(String msg)] }

let send_exn connection method_call exn =
  match OBus_error.unmake exn with
    | Some(name, msg) ->
        send_error connection method_call name msg
    | None ->
        Log.failure exn "sending an unregistred ocaml exception as a DBus error";
        send_error connection method_call "ocaml.Exception" (Printexc.to_string exn)

(***** Signal matching *****)

(* Matching on signals arguments *)
let rec tst_args args body = match args with
  | [] -> true
  | (n, p) :: rest -> tst_one_arg n p rest body

and tst_one_arg n p arest body = match n, body with
  | 0, Basic (String s) :: brest when s = p -> tst_args arest brest
  | n, _ :: brest -> tst_one_arg (n - 1) p arest brest
  | _ -> false

let signal_match r
    { sender = sender;
      typ = `Signal(path, interface, member);
      body = body } =
  (r.sr_sender = sender) &&
    (match r.sr_path with
       | Some p -> p = path
       | None -> true) &&
    (r.sr_interface = interface) &&
    (r.sr_member = member) &&
    (tst_args r.sr_args body)

let signal_match_ignore_sender r
    { typ = `Signal(path, interface, member);
      body = body } =
  (match r.sr_path with
     | Some p -> p = path
     | None -> true) &&
    (r.sr_interface = interface) &&
    (r.sr_member = member) &&
    (tst_args r.sr_args body)

let add_incoming_filter connection filter =
  with_running (fun connection -> (MSet.add connection.incoming_filters filter, connection.incoming_filters)) connection
let add_outgoing_filter connection filter =
  with_running (fun connection -> (MSet.add connection.outgoing_filters filter, connection.outgoing_filters)) connection

let filter_enabled (node, set) = not (MSet.is_alone node)
let enable_filter (node, set) = MSet.insert node set
let disable_filter (node, set) = MSet.remove node

(***** Reading/dispatching *****)

(* Find the handler for a reply and remove it. *)
let find_reply_waiter connection serial f g =
  match Serial_map.lookup serial connection.reply_waiters with
    | Some x ->
        connection.reply_waiters <- Serial_map.remove serial connection.reply_waiters;
        f x
    | None ->
        g ()

let ignore_send_exn connection method_call exn = ignore (send_exn connection method_call exn)

let unknown_method connection message =
  ignore_send_exn connection message (unknown_method_exn message)

let dispatch_message connection = function
    (* For method return and errors, we lookup at the reply
       waiters. If one is find then it get the reply, if none, then
       the reply is dropped. *)
  | { typ = `Method_return(reply_serial) } as message ->
      find_reply_waiter connection reply_serial
        (fun w -> wakeup w message)
        (fun _ -> Log.debug "reply to message with serial %ld dropped" reply_serial);

  | { typ = `Error(reply_serial, error_name) } as message ->
      let msg = get_error message in
      find_reply_waiter connection reply_serial
        (fun w -> wakeup_exn w (OBus_error.make error_name msg))
        (fun _ ->
           Log.debug "error reply to message with serial %ld dropped because no reply was expected, \
                      the error is: %S: %S" reply_serial error_name msg)

  | { typ = `Signal _ } as message ->
      begin match connection.name, message.sender with
        | None, _
        | _, None ->
            (* If this is a peer-to-peer connection, we do match on
               the sender *)
            MSet.iter
              (fun receiver ->
                 if signal_match_ignore_sender receiver message
                 then receiver.sr_handler connection message)
              connection.signal_receivers

        | Some _, Some _ ->
            MSet.iter
              (fun receiver ->
                 if signal_match receiver message
                 then receiver.sr_handler connection message)
              connection.signal_receivers
      end

  (* Hacks for the special "org.freedesktop.DBus.Peer" interface *)
  | { typ = `Method_call(_, Some "org.freedesktop.DBus.Peer", member); body = body } as message -> begin
      match member, body with
        | "Ping", [] ->
            (* Just pong *)
            ignore (dsend_reply connection message [])
        | "GetMachineId", [] ->
            ignore
              (try_bind (fun _ -> Lazy.force OBus_info.machine_uuid)
                 (fun machine_uuid -> send_reply connection message <:obus_type< string >> (OBus_uuid.to_string machine_uuid))
                 (fun exn -> perform
                    send_exn connection message (OBus_error.Failed "cannot get machine uuuid");
                    fail exn))
        | _ ->
            unknown_method connection message
    end

  | { typ = `Method_call(path, interface_opt, member) } as message ->
      match Object_map.lookup path connection.exported_objects with
        | Some obj ->
            begin try
              obj#obus_handle_call connection message
            with
                exn ->
                  Log.failure exn "method call handler failed with"
            end
        | None ->
            (* Handle introspection for missing intermediate object:

               for example if we have only one exported object with
               path "/a/b/c", we need to add introspection support for
               virtual objects with path "/", "/a", "/a/b",
               "/a/b/c". *)
            match
              match interface_opt, member with
                | None, "Introspect"
                | Some "org.freedesktop.DBus.Introspectable", "Introspect" ->
                    begin match children connection path with
                      | [] -> false
                      | l ->
                          ignore
                            (send_reply connection message <:obus_type< OBus_introspect.document >>
                               ([("org.freedesktop.DBus.Introspectable",
                                  [OBus_introspect.Method("Introspect", [],
                                                          [(None, Tbasic Tstring)], [])],
                                  [])], l));
                          true
                    end
                | _ -> false
            with
              | true -> ()
              | false ->
                  ignore_send_exn connection message
                    (OBus_error.Failed (sprintf "No such object: %S" (OBus_path.to_string path)))

let read_dispatch connection =
  catch
    (fun _ -> choose [OBus_lowlevel.recv connection.transport;
                      connection.abort])
    (fun exn ->
       fail (set_crash connection
               (match exn with
                  | End_of_file -> Connection_lost
                  | exn -> Transport_error exn)))
  >>= fun message ->

    if !(OBus_info.dump) then
      Format.eprintf "-----@\n@[<hv 2>message received:@\n%a@]@."
        OBus_message.print message;

    match apply_filters "incoming" message connection.incoming_filters with
      | None ->
          Log.debug "incoming message dropped by filters";
          return ()
      | Some message ->
          dispatch_message connection message;
          return ()

let rec dispatch_forever connection =
  try_bind
    (fun _ -> match connection.down with
       | Some w -> w >>= (fun _ -> read_dispatch connection)
       | None -> read_dispatch connection)
    (fun _ -> dispatch_forever connection)
    (function
       | Connection_closed -> return ()
       | exn ->
           try
             !(connection.on_disconnect) exn;
             return ()
           with
               exn ->
                 Log.failure exn "the error handler (OBus_connection.on_disconnect) failed with:";
                 return ())

let of_transport ?guid ?(up=true) transport =
  let make () =
    let abort = wait () in
    let connection = {
      crashed = None;
      transport = (transport :> OBus_lowlevel.transport);
      outgoing = Lwt.return 1l;
      reply_waiters = Serial_map.empty;
      signal_receivers = MSet.make ();
      exported_objects = Object_map.empty;
      incoming_filters = MSet.make ();
      outgoing_filters = MSet.make ();
      name = None;
      guid = guid;
      on_disconnect = ref (fun _ -> ());
      down = (match up with
                | true -> None
                | false -> Some(wait ()));
      abort = abort;
      watch = abort >>= (fun _ -> return ());
      shutdown_transport_on_close = ref true;
    } in
    ignore (dispatch_forever connection);
    connection
  in
  match guid with
    | None -> make ()
    | Some guid ->
        match Guid_map.lookup guid !guid_connection_map with
          | Some connection -> connection
          | None ->
              let connection = make () in
              guid_connection_map := Guid_map.add guid connection !guid_connection_map;
              connection

let is_up = with_running (fun connection -> connection.down = None)
let set_up =
  with_running begin fun connection ->
    match connection.down with
      | None -> ()
      | Some w ->
          connection.down <- None;
          wakeup w ()
  end
let set_down =
  with_running begin fun connection ->
    match connection.down with
      | Some _ -> ()
      | None -> connection.down <- Some(wait ())
  end

let of_addresses ?(shared=true) addresses = match shared with
  | false ->
      (perform
         (guid, transport) <-- OBus_lowlevel.transport_of_addresses addresses;
         return (of_transport transport))
  | true ->
      (* Try to find a guid that we already have *)
      let guids = Util.filter_map (fun ( _, g) -> g) addresses in
      match Util.find_map (fun guid -> Guid_map.lookup guid !guid_connection_map) guids with
        | Some connection -> return connection
        | None ->
            (* We ask again a shared connection even if we know that
               there is no other connection to a server with the same
               guid, because during the authentification another
               thread can add a new connection. *)
            (perform
               (guid, transport) <-- OBus_lowlevel.transport_of_addresses addresses;
               return (of_transport ~guid transport))

let loopback = of_transport (OBus_lowlevel.loopback ())

let on_disconnect = with_running (fun connection -> connection.on_disconnect)
let transport = with_running (fun connection -> connection.transport)
let name = with_running (fun connection -> connection.name)
let running connection = match connection.crashed with
  | None -> true
  | Some _ -> false
let watch connection = match connection.crashed with
  | None -> connection.watch
  | Some Connection_closed -> return ()
  | Some exn -> fail exn
let incoming_filters = with_running (fun connection -> connection.incoming_filters)
let outgoing_filters = with_running (fun connection -> connection.outgoing_filters)
