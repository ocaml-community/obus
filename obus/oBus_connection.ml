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
open OBus_value
open Lwt

type guid = OBus_address.guid
type name = string

exception Connection_closed
exception Connection_lost
exception Transport_error of exn

type t = OBus_internals.packed_connection

type filter = OBus_internals.filter
type filter_id = filter MSet.node

exception Context of t * OBus_message.any
let mk_context connection msg = Context(connection, (msg :> OBus_message.any))

let tt = OBus_type.wrap_sequence_ctx tunit
  (fun context () -> match context with
     | Context(connection, msg) -> connection
     | _ -> raise OBus_type.Cast_failure)
  (fun _ -> ())

(* Mapping from server guid to connection. *)
module Guid_map = Util.Make_map(struct type t = guid end)
let guid_connection_map = ref Guid_map.empty

(* Apply a list of filter on a message, logging failure *)
let apply_filters typ message filters =
  try
    MSet.filter filters message
  with
      exn ->
        Log.failure exn "%s filters failed with" typ;
        None

(* Get the error message of an error *)
let get_error msg = match msg.body with
  | Basic String x :: _ -> x
  | _ -> ""

(* Run [code] if [connection] contains a running connection, otherwise
   raise the exception to which [packed_connection] is set. *)
DEFINE EXEC(code) = (match connection#get with
                       | Crashed exn ->
                           raise exn
                       | Running connection ->
                           code)

(* Same as [EXEC] but use with [Lwt.fail] instead of [raise] *)
DEFINE LEXEC(code) = (match connection#get with
                        | Crashed exn ->
                            Lwt.fail exn
                        | Running connection ->
                            code)

(* +------------------+
   | Sending messages |
   +------------------+ *)

(* Send a message, maybe adding a reply waiter and return
   [return_thread] *)
let send_message_backend connection reply_waiter_opt return_thread message =
  EXEC(let current_outgoing = connection.outgoing in
       let w = wait () in
       connection.outgoing <- w;
       current_outgoing >>= fun serial ->
         match apply_filters "outgoing" { (message :> any) with serial = serial } connection.outgoing_filters with
           | None ->
               Log.debug "outgoing message dropped by filters";
               wakeup w serial;
               fail (Failure "message dropped by filters")

           | Some message ->
               begin match reply_waiter_opt with
                 | Some w ->
                     connection.reply_waiters <- Serial_map.add serial w connection.reply_waiters
                 | None ->
                     ()
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
                        let exn = connection.packed#set_crash (Transport_error exn) in
                        wakeup_exn w exn;
                        fail exn))

let send_message connection message =
  send_message_backend connection None (return ()) message

let send_message_with_reply connection message =
  let w = wait () in
  send_message_backend connection (Some w) w (message :> OBus_message.any)

let method_call' connection ?flags ?sender ?destination ~path ?interface ~member body ty_reply =
  send_message_with_reply connection (method_call ?flags ?sender ?destination ~path ?interface ~member body)
  >>= function
    | { typ = `Method_return _ } as msg ->
        begin try
          return (OBus_type.cast_sequence ~context:(mk_context connection msg) ty_reply msg.body)
        with
          | OBus_type.Cast_failure ->
              (* If not, check why the cast fail *)
              let expected_sig = OBus_type.type_sequence ty_reply
              and got_sig = type_of_sequence msg.body in
              if expected_sig = got_sig
              then
                (* If the signature match, this means that the user
                   defined a combinator raising a Cast_failure *)
                fail OBus_type.Cast_failure
              else
                (* In other case this means that the expected
                   signature is wrong *)
                fail
                  (Failure (sprintf "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                              member (match interface with Some i -> i | None -> "")
                              (string_of_signature expected_sig)
                              (string_of_signature got_sig)))
        end

    | { typ = `Error(_, error_name) } as msg ->
        fail (OBus_error.make error_name (get_error msg))

let method_call_no_reply connection ?(flags=default_flags) ?sender ?destination ~path ?interface ~member ty =
  OBus_type.make_func ty begin fun body ->
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
  OBus_type.make_func ty begin fun body ->
    method_call' connection ?flags ?sender ?destination ~path ?interface ~member body (OBus_type.func_reply ty)
  end

let emit_signal connection ?flags ?sender ?destination ~path ~interface ~member ty x =
  send_message connection (signal ?flags ?sender ?destination ~path ~interface ~member (OBus_type.make_sequence ty x))

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
  dsend_reply connection mc (OBus_type.make_sequence typ v)

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

let ignore_send_exn connection method_call exn = ignore(send_exn connection method_call exn)

let unknown_method connection message =
  ignore_send_exn connection message (unknown_method_exn message)

(* +-----------------+
   | Signal matching |
   +-----------------+ *)

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
  (match r.sr_sender, sender with
     | None, _ -> true

     (* this normally never happen because with a message bus, all
        messages have a sender field *)
     | _, None -> false

     (* This case is when the name the rule filter on do not currently
        have an owner *)
     | Some { contents = None }, _ -> false

     | Some { contents = Some owner }, Some sender -> owner = sender) &&
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

(* +---------------------+
   | Reading/dispatching |
   +---------------------+ *)

let dispatch_message connection = function
    (* For method return and errors, we lookup at the reply
       waiters. If one is find then it get the reply, if none, then
       the reply is dropped. *)
  | { typ = `Method_return(reply_serial) }
  | { typ = `Error(reply_serial, _) } as message ->
      begin match Serial_map.lookup reply_serial connection.reply_waiters with
        | Some w ->
            connection.reply_waiters <- Serial_map.remove reply_serial connection.reply_waiters;
            wakeup w message

        | None ->
            Log.debug "reply to message with serial %ld dropped%s"
              reply_serial (match message with
                              | { typ = `Method_return _ } ->
                                  ""
                              | { typ = `Error(_, error_name) } as message ->
                                  sprintf ", the reply is the error: %S: %S"
                                    error_name (get_error message))
      end

  | { typ = `Signal _ } as message ->
      begin match connection.name, message.sender with
        | None, _
        | _, None ->
            (* If this is a peer-to-peer connection, we do match on
               the sender *)
            MSet.iter
              (fun receiver ->
                 if signal_match_ignore_sender receiver message
                 then callback_apply "signal callback" receiver.sr_callback (connection.packed, message))
              connection.signal_receivers

        | Some _, Some sender ->
            begin match sender, message with

              (* Internal handling of "NameOwnerChange" messages for
                 name resolving. *)
              | "org.freedesktop.DBus",
                { typ = `Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameOwnerChanged");
                  body = [Basic(String name); Basic(String old_owner); Basic(String new_owner)] } ->

                  let owner = if new_owner = "" then None else Some new_owner in

                  if OBus_name.is_unique name && owner = None then
                    (* If the resovler was monitoring a unique name
                       and it is not owned anymore, this means that
                       the peer with this name has exited. We remember
                       this information here. *)
                    Cache.add connection.exited_peers name;

                  begin match Name_map.lookup name connection.name_resolvers with
                    | Some nr ->
                        Log.debug "updating internal name resolver: %S -> %S" name (match owner with
                                                                                      | Some n -> n
                                                                                      | None -> "");
                        nr.nr_owner := owner;

                        if not nr.nr_initialized then begin
                          (* The resolver has not yet been
                             initialized; this means that the reply to
                             GetNameOwner (done by
                             [OBus_resolver.make]) has not yet been
                             received. We consider that this first
                             signal has precedence and terminate
                             initialization. *)
                          nr.nr_initialized <- true;

                          (* Wakeup threads waiting for
                             initialization *)
                          Lwt.wakeup nr.nr_init ()
                        end;

                        MSet.iter (fun ncc -> call_resolver_handler ncc owner) nr.nr_on_change
                    | None ->
                        ()
                  end

              (* Internal handling of "NameAcquired" signals *)
              | ("org.freedesktop.DBus",
                 { typ = `Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameAcquired");
                   body = [Basic(String name)] })

                  (* Only handle signals destined to us *)
                  when message.destination = connection.name ->

                  connection.acquired_names <- name :: connection.acquired_names

              (* Internal handling of "NameLost" signals *)
              | ("org.freedesktop.DBus",
                 { typ = `Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameLost");
                   body = [Basic(String name)] })

                  (* Only handle signals destined to us *)
                  when message.destination = connection.name ->

                  connection.acquired_names <- List.filter ((<>) name) connection.acquired_names

              | _ ->
                  ()
            end;

            (* Only handle signals broadcasted destined to us *)
            if message.destination = None || message.destination = connection.name then
              MSet.iter
                (fun receiver ->
                   if signal_match receiver message
                   then callback_apply "signal callback" receiver.sr_callback (connection.packed, message))
                connection.signal_receivers
      end

  (* Handling of the special "org.freedesktop.DBus.Peer" interface *)
  | { typ = `Method_call(_, Some "org.freedesktop.DBus.Peer", member); body = body } as message -> begin
      match member, body with
        | "Ping", [] ->
            (* Just pong *)
            ignore (dsend_reply connection.packed message [])
        | "GetMachineId", [] ->
            ignore
              (try_bind (fun _ -> Lazy.force OBus_info.machine_uuid)
                 (fun machine_uuid -> send_reply connection.packed message <:obus_type< string >> (OBus_uuid.to_string machine_uuid))
                 (fun exn -> perform
                    send_exn connection.packed message (OBus_error.Failed "cannot get machine uuuid");
                    fail exn))
        | _ ->
            unknown_method connection.packed message
    end

  | { typ = `Method_call(path, interface_opt, member) } as message ->
      match Object_map.lookup path connection.exported_objects with
        | Some obj ->
            begin try
              obj#obus_handle_call connection.packed message
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
                            (send_reply connection.packed message <:obus_type< OBus_introspect.document >>
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
                  ignore_send_exn connection.packed message
                    (OBus_error.Failed (sprintf "No such object: %S" (OBus_path.to_string path)))

let read_dispatch connection =
  catch
    (fun _ -> choose [OBus_lowlevel.recv connection.transport;
                      connection.abort])
    (fun exn ->
       fail (connection.packed#set_crash
               (match exn with
                  | End_of_file -> Connection_lost
                  | OBus_lowlevel.Protocol_error _ as exn -> exn
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

(* +-----------------------+
   | ``Packed'' connection |
   +-----------------------+ *)

class packed_connection = object

  val mutable state = Crashed Exit (* Fake initial state *)

  (* Set the initial running state *)
  method set_connection connection =
    state <- Running connection

  method get = state

  (* Put the connection in a "crashed" state. This means that all
     subsequent call using the connection will fail. *)
  method set_crash exn = match state with
    | Crashed exn ->
        exn
    | Running connection ->
        state <- Crashed exn;

        begin match connection.guid with
          | Some guid -> guid_connection_map := Guid_map.remove guid !guid_connection_map
          | None -> ()
        end;

        (* This make the dispatcher to exit if it is waiting on
           [get_message] *)
        wakeup_exn connection.abort exn;
        begin match connection.down with
          | Some w -> wakeup_exn w exn
          | None -> ()
        end;

        (* Shutdown the transport *)
        if !(connection.shutdown_transport_on_close) then
          (try
             OBus_lowlevel.shutdown connection.transport
           with
               exn ->
                 Log.failure exn "failed to abort/shutdown the transport");

        (* Wakeup all reply handlers so they will not wait forever *)
        Serial_map.iter (fun _ w -> wakeup_exn w exn) connection.reply_waiters;

        (* Remove all objects *)
        Object_map.iter begin fun p obj ->
          try
            obj#obus_connection_closed connection.packed
          with
              exn ->
                (* This may happen if the programmer has overridden the
                   method *)
                Log.failure exn "obus_connection_closed on object with path %S failed with"
                  (OBus_path.to_string p)
        end connection.exported_objects;

        exn
end

(* +---------------------+
   | Connection creation |
   +---------------------+ *)

let of_transport ?guid ?(up=true) transport =
  let make _ =
    let abort = Lwt.wait () and packed_connection = new packed_connection in
    let connection = {
      name = None;
      acquired_names = [];
      transport = transport;
      shutdown_transport_on_close = ref false;
      on_disconnect = ref (fun _ -> ());
      guid = guid;
      down = (if up then None else Some(Lwt.wait ()));
      abort = abort;
      watch = try_bind (fun _ -> abort)
        (fun _ -> return ())
        (function
           | Connection_closed -> return ()
           | exn -> fail exn);
      name_resolvers = Name_map.empty;
      exited_peers = Cache.create 100;
      outgoing = Lwt.return 1l;
      exported_objects = Object_map.empty;
      incoming_filters = MSet.make ();
      outgoing_filters = MSet.make ();
      reply_waiters = Serial_map.empty;
      signal_receivers = MSet.make ();
      packed = (packed_connection :> t);
    } in
    packed_connection#set_connection connection;
    (* Start the dispatcher *)
    ignore (dispatch_forever connection);
    connection.packed
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

(* +-------+
   | Other |
   +-------+ *)

let running connection = match connection#get with
  | Running _ -> true
  | Crashed _ -> false

let watch connection = LEXEC(connection.watch)

DEFINE GET(param) = (fun connection -> EXEC(connection.param))

let guid = GET(guid)
let transport = GET(transport)
let name = GET(name)
let on_disconnect = GET(on_disconnect)
let shutdown_transport_on_close = GET(shutdown_transport_on_close)
let close connection = EXEC(ignore (connection.packed#set_crash Connection_closed))

let is_up connection =
  EXEC(connection.down = None)

let set_up connection =
  EXEC(match connection.down with
         | None -> ()
         | Some w ->
             connection.down <- None;
             wakeup w ())

let set_down connection =
  EXEC(match connection.down with
         | Some _ -> ()
         | None -> connection.down <- Some(wait ()))

let add_incoming_filter connection filter =
  EXEC(MSet.add connection.incoming_filters filter)

let add_outgoing_filter connection filter =
  EXEC(MSet.add connection.outgoing_filters filter)

let remove_filter = MSet.remove
