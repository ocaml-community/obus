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

type filter_id = filter MSet.node
type signal_receiver = (signal_match_rule * signal handler) MSet.node

exception Connection_closed
exception Connection_lost

type t = connection

(* Mapping from server guid to connection. *)
module Guid_map = My_map(struct type t = guid end)
let guid_connection_map = ref Guid_map.empty

let remove_connection_of_guid_map = function
  | { guid = Some guid } -> guid_connection_map := Guid_map.remove guid !guid_connection_map
  | { guid = None } -> ()

(***** Error handling *****)

(* Put the connection in a "crashed" state. This means that all
   subsequent call using the connection will fail. *)
let set_crash connection exn = match !connection with
  | Crashed exn -> (true, exn)
  | Running running ->
      connection := Crashed exn;
      remove_connection_of_guid_map running;

      (* This make the dispatcher to exit if it is waiting on
         [get_message] *)
      wakeup_exn running.abort exn;

      (* Abort and shutdown the transport *)
      (try
         running.transport#abort exn;
         running.transport#shutdown
       with
           exn ->
             Log.debug "fail to abort/shutdown the transport: %s"
               (Printexc.to_string exn));

      (* Remove all objects *)
      Object_map.iter begin fun p obj ->
        try
          obj#obus_connection_closed connection
        with
            exn ->
              (* This may happen if the programmer has overridden
                 the method *)
              Log.debug "obus_connection_closed on object with path %S fail with: %s"
                (OBus_path.to_string p) (Printexc.to_string exn)
      end running.exported_objects;

      (* Wakeup all reply handlers so they will not wait forever *)
      Serial_map.iter (fun _ w -> wakeup_exn w exn) running.reply_waiters;
      (false, exn)

let check_crash connection = match !connection with
  | Crashed exn -> raise exn
  | _ -> ()

let close connection = match set_crash connection Connection_closed with
  | true, exn -> raise exn
  | _ -> ()

(* Get the error message of an error *)
let get_error msg = match msg.body with
  | Basic String x :: _ -> x
  | _ -> ""

(***** Sending messages *****)

let send_message_backend reply_waiter return_thread connection message =
  lwt_with_running connection begin fun running ->
    let outgoing = running.outgoing in
    let w = wait () in
    running.outgoing <- w;
    outgoing >>= fun serial ->
      let serial = Int32.succ serial in
      let message = { message with serial = serial } in

      (match reply_waiter with
         | Some w ->
             running.reply_waiters <- Serial_map.add serial w running.reply_waiters
         | None -> ());

      if OBus_info.dump then
        Format.eprintf "-----@\n@[<hv 2>sending message:@\n%a@]@."
          OBus_message.print message;

      match running.transport#put_message (message :> OBus_message.any) with
        | OBus_lowlevel.Marshaler_failure msg ->
            wakeup w serial;
            fail (Failure ("can not send message: " ^ msg))

        | OBus_lowlevel.Marshaler_success f ->
            try_bind f
              (fun _ ->
                 wakeup w serial;
                 return_thread)
              (fun exn ->
                 (* Any error is fatal here, because this is possible
                    that a message has been partially sent on the
                    connection, so the message stream is broken *)
                 let exn = snd (set_crash connection exn) in
                 wakeup_exn w exn;
                 fail exn)
  end

let send_message connection message =
  send_message_backend None (return ()) connection message

let send_message_with_reply connection message =
  let w = wait () in
  send_message_backend (Some w) w connection message

(***** Helpers *****)

exception Context of connection * OBus_message.any
let mk_context connection msg = Context(connection, (msg :> OBus_message.any))

let call_and_cast_reply ty cont =
  make_func ty begin fun body ->
    cont body begin fun connection message ->
      send_message_with_reply connection message >>= fun msg ->
        match opt_cast_sequence ~context:(mk_context connection msg) (func_reply ty) msg.body with

          (* If the cast success, just return the result *)
          | Some x -> return x

          (* If not, check why the cast fail *)
          | None ->
              let expected_sig = osignature ty
              and got_sig = type_of_sequence msg.body in
              if expected_sig = got_sig
              then
                (* If the signature match, this means that the
                   user defined a combinator raising a
                   Cast_failure *)
                fail Cast_failure
              else
                (* In other case this means that the expected
                   signature is wrong *)
                let { typ = `Method_call(path, interf, member) } = message in
                fail
                  (Failure (sprintf "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                              member (match interf with Some i -> i | None -> "")
                              (string_of_signature expected_sig)
                              (string_of_signature got_sig)))
    end
  end

let dmethod_call connection ?flags ?sender ?destination ~path ?interface ~member body =
  send_message_with_reply connection
    (method_call ?flags ?sender ?destination ~path ?interface ~member body)
  >>= fun { body = x } -> return x

let kmethod_call cont ?flags ?sender ?destination ~path ?interface ~member ty =
  call_and_cast_reply ty begin fun body f ->
    cont (fun connection ->
            f connection (method_call ?flags ?sender ?destination ~path ?interface ~member body))
  end

let method_call connection ?flags ?sender ?destination ~path ?interface ~member ty =
  call_and_cast_reply ty begin fun body f ->
    f connection (method_call ?flags ?sender ?destination ~path ?interface ~member body)
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
        send_error connection method_call "org.freedesktop.DBus.Error.Failed"
          (sprintf "uncaught ocaml exception: %s" (Printexc.to_string exn))

(***** Signals and filters *****)

let add_signal_receiver connection ?sender ?destination ?path ?interface ?member ?(args=[]) typ func =
  with_running connection begin fun running ->
    MSet.add running.signal_handlers
      ({ smr_sender = sender;
         smr_destination = destination;
         smr_path = path;
         smr_interface = interface;
         smr_member = member;
         smr_args = make_args_filter args },
       fun msg -> ignore(match opt_cast_sequence typ ~context:(mk_context connection msg) msg.body with
                           | Some x -> func x
                           | None -> ()))
  end

let dadd_signal_receiver connection ?sender ?destination ?path ?interface ?member ?(args=[]) func =
  with_running connection begin fun running ->
    MSet.add running.signal_handlers
      ({ smr_sender = sender;
         smr_destination = destination;
         smr_path = path;
         smr_interface = interface;
         smr_member = member;
         smr_args = make_args_filter args },
       fun msg -> func msg.body)
  end

let add_filter connection filter =
  with_running connection (fun running -> MSet.add running.filters filter)

let signal_receiver_enabled = MSet.enabled
let enable_signal_receiver = MSet.enable
let disable_signal_receiver = MSet.disable

let filter_enabled = MSet.enabled
let enable_filter = MSet.enable
let disable_filter = MSet.disable

(***** Reading/dispatching *****)

(* Find the handler for a reply and remove it. *)
let find_reply_waiter running serial f g =
  match Serial_map.lookup serial running.reply_waiters with
    | Some x ->
        running.reply_waiters <- Serial_map.remove serial running.reply_waiters;
        f x
    | None ->
        g ()

let ignore_send_exn connection method_call exn = ignore (send_exn connection method_call exn)

let unknown_method connection message =
  ignore_send_exn connection message (unknown_method_exn message)

let dispatch_message connection running message =
  let call typ f message =
    begin try
      f message
    with
        exn ->
          Log.debug "%s failed with this exception: %s" typ (Printexc.to_string exn)
    end;
    (* The connection may have crash during the execution of f *)
    check_crash connection
  in

  (* First of all, pass the message through all filters *)
  MSet.iter (fun filter -> call "filter" filter message) running.filters;

  (* Now we do the specific dispatching *)
  match message with

    (* For method return and errors, we lookup at the reply
       waiters. If one is find then it get the reply, if none, then
       the reply is dropped. *)
    | { typ = `Method_return(reply_serial) } as message ->
        find_reply_waiter running reply_serial
          (fun w -> wakeup w message)
          (fun _ -> Log.debug "reply to message with serial %ld dropped" reply_serial)

    | { typ = `Error(reply_serial, error_name) } ->
        let msg = get_error message in
        find_reply_waiter running reply_serial
          (fun w -> wakeup_exn w (OBus_error.make error_name msg))
          (fun _ ->
             Log.debug "error reply to message with serial %ld dropped because no reply was expected, \
                        the error is: %S: %S" reply_serial error_name msg)

    | { typ = `Signal _ } as message ->
        MSet.iter
          (fun (match_rule, handler) ->
             if signal_match match_rule message
             then call "signal handler" handler message)
          running.signal_handlers

    (* Hacks for the special "org.freedesktop.DBus.Peer" interface *)
    | { typ = `Method_call(_, Some "org.freedesktop.DBus.Peer", member); body = body } as message -> begin
        match member, body with
          | "Ping", [] ->
              (* Just pong *)
              ignore (dsend_reply connection message [])
          | "GetMachineId", [] ->
              let machine_uuid = Lazy.force OBus_info.machine_uuid in
              ignore (dsend_reply connection message [vbasic(String machine_uuid)])
          | _ ->
              unknown_method connection message
      end

    | { typ = `Method_call(path, interface_opt, member) } as message ->
        match Object_map.lookup path running.exported_objects with
          | Some obj -> call "method call" (fun msg -> obj#obus_handle_call connection msg) message
          | None ->
              (* Handle introspection for missing intermediate object:

                 for example if we have only one exported object with
                 path "/a/b/c", we need to add introspection support
                 for virtual objects with path "/", "/a", "/a/b",
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
                                    [OBus_interface.Method("Introspect", [],
                                                           [(None, Tbasic Tstring)], [])],
                                    [])], l));
                            true
                      end
                  | _ -> false
              with
                | true -> ()
                | false ->
                    ignore_send_exn connection message (OBus_error.Failed (sprintf "No such object: %S" (OBus_path.to_string path)))

let read_dispatch connection running =
  bind
    (catch
       (fun _ -> choose [running.transport#get_message;
                         running.abort])
       (function
          | End_of_file
          | OBus_lowlevel.Transport_error End_of_file -> fail Connection_lost
          | OBus_lowlevel.Transport_error _
          | OBus_lowlevel.Protocol_error _ as exn -> fail exn
          | exn -> fail (OBus_lowlevel.Transport_error exn)))
    (fun message ->
       if OBus_info.dump then
         Format.eprintf "-----@\n@[<hv 2>message received:@\n%a@]@."
           OBus_message.print message;
       try
         dispatch_message connection running message;
         return ()
       with
           _ ->
             (* The exception has been raised by check_crash, so we
                can ignore it here *)
             return ())

let rec dispatch_forever connection running =
  try_bind
    (fun _ -> match running.down with
       | Some w -> w >>= (fun _ -> read_dispatch connection running)
       | None -> read_dispatch connection running)
    (fun _ -> dispatch_forever connection running)
    (function
       | Connection_closed -> return ()
       | exn ->
           try
             !(running.on_disconnect) exn;
             return ()
           with
               exn ->
                 Log.debug "the error handler (OBus_connection.on_disconnect) failed with this exception: %s" (Printexc.to_string exn);
                 return ())

let of_transport ?guid ?down transport =
  let make () =
    let running = {
      transport = (transport :> OBus_lowlevel.transport);
      outgoing = Lwt.return 0l;
      reply_waiters = Serial_map.empty;
      signal_handlers = MSet.make ();
      exported_objects = Object_map.empty;
      filters = MSet.make ();
      name = None;
      guid = guid;
      on_disconnect = ref (fun _ -> ());
      down = down;
      abort = wait ();
    } in
    let connection = ref (Running running) in
    ignore (dispatch_forever connection running);
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

let of_client_transport ?(shared=true) transport =
  (perform
     guid <-- Lazy.force (transport#client_authenticate);
     let down = wait () in
     return (if shared then
               of_transport ~down ~guid transport
             else
               of_transport ~down transport))

let of_server_transport transport =
  (perform
     Lazy.force (transport#server_authenticate);
     return (of_transport ~down:(wait ()) transport))

let is_up connection =
  with_running connection (fun running -> running.down = None)
let set_up connection =
  with_running connection begin fun running ->
    match running.down with
      | None -> ()
      | Some w ->
          running.down <- None;
          wakeup w ()
  end
let set_down connection =
  with_running connection begin fun running ->
    match running.down with
      | Some _ -> ()
      | None -> running.down <- Some(wait ())
  end

let of_addresses ?(shared=true) addresses = match shared with
  | false ->
      (perform
         transport <-- OBus_lowlevel.client_transport_of_addresses addresses;
         Lazy.force (transport#client_authenticate);
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
               transport <-- OBus_lowlevel.client_transport_of_addresses addresses;
               guid <-- Lazy.force (transport#client_authenticate);
               return (of_transport ~guid transport))

let loopback = of_transport (new OBus_lowlevel.loopback)

let on_disconnect connection =
  with_running connection (fun running -> running.on_disconnect)
let transport connection =
  with_running connection (fun running -> running.transport)
let name connection =
  with_running connection (fun running -> running.name)
let running connection = match !connection with
  | Running _ -> true
  | Crashed _ -> false
let watch connection = match !connection with
  | Running running -> running.abort >>= (fun _ -> return ())
  | Crashed Connection_closed -> return ()
  | Crashed exn -> fail exn
