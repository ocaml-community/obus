(*
 * oBus_connection.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(connection)"

open Lwt
open OBus_message
open OBus_private_connection
open OBus_value

exception Connection_closed = OBus_private_connection.Connection_closed
exception Connection_lost = OBus_private_connection.Connection_lost
exception Transport_error = OBus_private_connection.Transport_error

let () =
  Printexc.register_printer
    (function
       | Connection_closed ->
           Some "D-Bus connection closed"
       | Connection_lost ->
           Some "D-Bus connection lost"
       | Transport_error exn ->
           Some(Printf.sprintf "D-Bus transport failure: %s" (Printexc.to_string exn))
       | _ ->
           None)

type t = OBus_private_connection.t

let compare = Pervasives.compare

type filter = OBus_private_connection.filter

(* Mapping from server guid to connection. *)
module Guid_map = Map.Make(struct
                            type t = OBus_address.guid
                            let compare = Pervasives.compare
                          end)
let guid_connection_map = ref Guid_map.empty

let send_message = OBus_private_connection.send_message
let send_message_with_reply = OBus_private_connection.send_message_with_reply

(* +-----------------------------------------------------------------+
   | Signal matching                                                 |
   +-----------------------------------------------------------------+ *)

let match_sender receiver message =
  match receiver.receiver_sender, message.sender with
    | None, _ ->
        true

    | Some _, None ->
        (* this normally never happen because with a message bus, all
           messages have a sender field *)
        false

    | Some name, Some sender ->
        match React.S.value name with
          | None ->
              (* This case is when the name the rule filter on do not
                 currently have an owner *)
              false

          | Some owner ->
              owner = sender

(* +-----------------------------------------------------------------+
   | Reading/dispatching                                             |
   +-----------------------------------------------------------------+ *)

let introspection children =
  let buffer = Buffer.create 42 in
  Buffer.add_string buffer "<node>\n";
  List.iter
    (fun child ->
       Buffer.add_string buffer "  <node name=\"";
       Buffer.add_string buffer child;
       Buffer.add_string buffer "\"/>\n")
    children;
  Buffer.add_string buffer "</node>\n";
  Buffer.contents buffer

let reply_expected message =
  match message .typ with
    | Method_call(path, interface_opt, member) ->
        Lwt_log.error_f ~section "no reply sent by %S on interface %S, but one was expected"
          member (match interface_opt with None -> "" | Some name -> name)
    | _ ->
        return ()

let dispatch_message connection running message = match message with

  (* For method return and errors, we lookup at the reply waiters. If
     one is find then it get the reply, if none, then the reply is
     dropped. *)
  | { typ = Method_return(reply_serial) }
  | { typ = Error(reply_serial, _) } -> begin
      match try Some(Serial_map.find reply_serial running.running_reply_waiters) with Not_found -> None with
        | Some w ->
            running.running_reply_waiters <- Serial_map.remove reply_serial running.running_reply_waiters;
            wakeup w message;
            return ()
        | None ->
            Lwt_log.debug_f ~section "reply to message with serial %ld dropped%s"
              reply_serial (match message with
                              | { typ = Error(_, error_name) } ->
                                  Printf.sprintf ", the reply is the error: %S: %S"
                                    error_name
                                    (match message.body with
                                       | V.Basic (V.String x) :: _ -> x
                                       | _ -> "")
                              | _ ->
                                  "")
    end

  | { typ = Signal(path, interface, member) } -> begin
      let context = make_context connection message in
      match running.running_name, message.sender with
        | None, _ -> begin
            (* If this is a peer-to-peer connection, we do not match
               on the sender *)
            match try Some(Signal_map.find (path, interface, member) running.running_receiver_groups) with Not_found -> None with
              | Some receiver_group ->
                  Lwt_sequence.fold_l
                    (fun receiver thread ->
                       join [
                         thread;
                         if receiver.receiver_active && receiver.receiver_filter message then
                           try_lwt
                             receiver.receiver_push (context, message);
                             return ()
                           with exn ->
                             Lwt_log.error ~section ~exn "signal event failed with"
                         else
                           return ()
                       ])
                    receiver_group.receiver_group_receivers (return ())
              | None ->
                  return ()
          end

        | Some _, None ->
            Lwt_log.error_f ~section "signal without sender received from message bus"

        | Some _, Some sender ->
            lwt () = match sender, message with

              (* Internal handling of "NameOwnerChange" messages for
                 name resolving. *)
              | "org.freedesktop.DBus",
                { typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameOwnerChanged");
                  body = [V.Basic(V.String name); V.Basic(V.String old_owner); V.Basic(V.String new_owner)] } ->

                  let owner = if new_owner = "" then None else Some new_owner in

                  if OBus_name.is_unique name && owner = None then
                    (* If the resovler was monitoring a unique name
                       and it is not owned anymore, this means that
                       the peer with this name has exited. We remember
                       this information here. *)
                    OBus_cache.add running.running_exited_peers name;

                  begin match try Some(Name_map.find name running.running_resolvers) with Not_found -> None with
                    | Some resolver ->
                        lwt () =
                          Lwt_log.debug_f ~section "updating internal name resolver: %S -> %S"
                            name
                            (match owner with
                               | Some n -> n
                               | None -> "")
                        in

                        resolver.resolver_set owner;

                        begin
                          match resolver.resolver_state with
                            | Resolver_init(waiter, wakener) ->
                                (* The resolver has not yet been
                                   initialized; this means that the
                                   reply to GetNameOwner (done by
                                   [OBus_resolver.make]) has not yet
                                   been received. We consider that
                                   this first signal has precedence
                                   and terminate initialization. *)
                                resolver.resolver_state <- Resolver_running;
                                Lwt.wakeup wakener resolver
                            | Resolver_running ->
                                ()
                        end;
                        return ()

                    | None ->
                        return ()
                  end

              (* Internal handling of "NameAcquired" signals *)
              | ("org.freedesktop.DBus",
                 { typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameAcquired");
                   body = [V.Basic(V.String name)] })

                  (* Only handle signals destined to us *)
                  when message.destination = running.running_name ->

                  running.running_set_acquired_names (Name_set.add name (React.S.value running.running_acquired_names));
                    return ()

              (* Internal handling of "NameLost" signals *)
              | ("org.freedesktop.DBus",
                 { typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameLost");
                   body = [V.Basic(V.String name)] })

                  (* Only handle signals destined to us *)
                  when message.destination = running.running_name ->

                  running.running_set_acquired_names (Name_set.remove name (React.S.value running.running_acquired_names));
                    return ()

              | _ ->
                  return ()
            in

            (* Only handle signals broadcasted or destined to us *)
            if message.destination = None || message.destination = running.running_name then
              match try Some(Signal_map.find (path, interface, member) running.running_receiver_groups) with Not_found -> None with
                | Some receiver_group ->
                    Lwt_sequence.fold_l
                      (fun receiver thread ->
                         join [
                           thread;
                           if receiver.receiver_active && match_sender receiver message && receiver.receiver_filter message then
                             try_lwt
                               receiver.receiver_push (context, message);
                               return ()
                             with exn ->
                               Lwt_log.error ~section ~exn "signal event failed with"
                           else
                             return ()
                         ])
                      receiver_group.receiver_group_receivers (return ())
                | None ->
                    return ()
            else
              return ()
    end

  (* Handling of the special "org.freedesktop.DBus.Peer" interface *)
  | { typ = Method_call(_, Some "org.freedesktop.DBus.Peer", member); body = body } -> begin
      let context = make_context_with_reply connection message in
      match member, body with
        | "Ping", [] ->
            send_reply context []
        | "GetMachineId", [] ->
            try_bind
              (fun () ->
                 Lazy.force OBus_info.machine_uuid)
              (fun machine_uuid ->
                 send_reply context [V.basic_string (OBus_uuid.to_string machine_uuid)])
              (fun exn ->
                 send_error context (OBus_error.Failed "cannot get machine uuuid"))
        | _ ->
            send_error context (OBus_error.Unknown_method (unknown_method_message message))
    end

  | { typ = Method_call(path, interface_opt, member); body = body } ->
      let context = make_context_with_reply connection message in
      (* Look in static objects *)
      begin
        try
          return (`Object(Object_map.find path running.running_static_objects))
        with Not_found ->
          (* Look in dynamic objects *)
          match
            Object_map.fold
              (fun prefix dynamic_object acc ->
                 match acc with
                   | Some _ ->
                       acc
                   | None ->
                       match OBus_path.after prefix path with
                         | Some path ->
                             Some(path, dynamic_object)
                         | None ->
                             None)
              running.running_dynamic_objects None
          with
            | None ->
                return `Not_found
            | Some(path, dynamic_object) ->
                try_lwt
                  dynamic_object context path
                with exn ->
                  lwt () = Lwt_log.error ~section ~exn "dynamic object handler failed with" in
                  return `Not_found
      end >>= function
        | `Object static_object -> begin
            lwt result =
              try_lwt
                (static_object.static_object_handle context message :> [ `Replied | `No_reply | `Failure of exn ] Lwt.t)
              with exn ->
                return (`Failure exn)
            in
            match result with
              | `Replied ->
                  return ()
              | `No_reply ->
                  if OBus_message.no_reply_expected context.context_flags then
                    return ()
                  else
                    reply_expected message
              | `Failure exn ->
                  lwt () =
                    if OBus_error.name exn = OBus_error.ocaml then
                      match interface_opt with
                        | Some interface ->
                            Lwt_log.error_f ~section ~exn
                              "method call handler for method %S on interface %S failed with"
                              member interface
                        | None ->
                            Lwt_log.error_f ~section ~exn
                              "method call handler for method %S failed with" member
                    else
                      return ()
                  in
                  send_error context exn
          end
        | `Replied ->
            return ()
        | `No_reply ->
            if OBus_message.no_reply_expected context.context_flags then
              return ()
            else
              reply_expected message
        | `Not_found ->
            (* Handle introspection for missing intermediate object:

               for example if we have only one exported object with
               path "/a/b/c", we need to add introspection support for
               virtual objects with path "/", "/a", "/a/b",
               "/a/b/c". *)
            match interface_opt, member, body with
              | None, "Introspect", []
              | Some "org.freedesktop.DBus.Introspectable", "Introspect", [] ->
                  send_reply context [V.basic_string (introspection (children running path))]
              | _ ->
                  send_error context (OBus_error.Failed (Printf.sprintf "No such object: %S" (OBus_path.to_string path)))

let rec dispatch_forever connection running =
  try_bind
    (fun () ->
       lwt () =
         match React.S.value running.running_down with
           | Some(waiter, wakener) ->
               waiter
           | None ->
               return ()
       in
       lwt message =
         try_lwt
           choose [OBus_transport.recv running.running_transport;
                   running.running_abort_recv]
         with exn ->
           fail =<< (connection#set_crash
                       (match exn with
                          | End_of_file -> Connection_lost
                          | OBus_wire.Protocol_error _ as exn -> exn
                          | exn -> Transport_error exn))
       in
       match apply_filters "incoming" message running.running_incoming_filters with
         | None ->
             lwt () = Lwt_log.debug ~section "incoming message dropped by filters" in
             return ()
         | Some message ->
             ignore (
               try_lwt
                 dispatch_message connection running message
               with exn ->
                 Lwt_log.error ~section ~exn "message dispatching failed with"
               finally
                 OBus_value.V.sequence_close message.body;
                 return ()
             );
             return ())
    (fun () ->
       dispatch_forever connection running)
    (function
       | Connection_closed ->
           return ()
       | exn ->
           (* At this state of the program the connection state should
              have been already set to [Crashed].

              The only possible error maybe an [Out_of_memory].
           *)
           lwt exn = connection#set_crash exn in
           try
             !(running.running_on_disconnect) exn;
             return ()
           with exn ->
             Lwt_log.error ~section ~exn "the error handler (OBus_connection.on_disconnect) failed with")

(* +-----------------------------------------------------------------+
   | ``Packed'' connection                                           |
   +-----------------------------------------------------------------+ *)

class connection =
  let running, set_running = React.S.create true in
object(self)

  method running = running

  val mutable state = Crashed Exit (* Fake initial state *)

  (* Set the initial running state *)
  method set_running running =
    state <- Running running

  method get = state

  (* Put the connection in a "crashed" state. This means that all
     subsequent call using the connection will fail. *)
  method set_crash exn = match state with
    | Crashed exn ->
        return exn
    | Running running ->
        state <- Crashed exn;
        set_running false;

        begin match running.running_guid with
          | Some guid -> guid_connection_map := Guid_map.remove guid !guid_connection_map
          | None -> ()
        end;

        (* This make the dispatcher to exit if it is waiting on
           [get_message] *)
        wakeup_exn running.running_abort_recv_wakener exn;
        begin match React.S.value running.running_down with
          | Some(waiter, wakener) -> wakeup_exn wakener exn
          | None -> ()
        end;

        (* Wakeup all reply handlers so they will not wait forever *)
        Serial_map.iter (fun _ w -> wakeup_exn w exn) running.running_reply_waiters;

        (* Remove all objects *)
        Object_map.iter
          (fun path static_object ->
             static_object.static_object_connection_closed running.running_wrapper)
          running.running_static_objects;

        (* If the connection is closed normally, flush it *)
        lwt () =
          if exn = Connection_closed then
            Lwt_mutex.with_lock running.running_outgoing_m return
          else begin
            wakeup_exn running.running_abort_send_wakener exn;
            return ()
          end
        in

        (* Shutdown the transport *)
        lwt () =
            try_lwt
              OBus_transport.shutdown running.running_transport
            with exn ->
              Lwt_log.error ~section ~exn "failed to abort/shutdown the transport"
        in
        return exn
end

(* +-----------------------------------------------------------------+
   | Connection creation                                             |
   +-----------------------------------------------------------------+ *)

let of_transport ?guid ?(up=true) transport =
  let make () =
    let abort_recv, abort_recv_wakener = Lwt.wait ()
    and abort_send, abort_send_wakener = Lwt.wait ()
    and connection = new connection
    and down, set_down = React.S.create (if up then None else Some(wait ()))
    and acquired_names, set_acquired_names = React.S.create ~eq:Name_set.equal Name_set.empty in
    let state = React.S.map (function None -> `Up | Some _ -> `Down) down in
    let running = {
      running_name = None;
      running_acquired_names = acquired_names;
      running_set_acquired_names = set_acquired_names;
      running_transport = transport;
      running_on_disconnect = ref (fun _ -> ());
      running_guid = guid;
      running_down = down;
      running_set_down = set_down;
      running_state = state;
      running_abort_recv = abort_recv;
      running_abort_send = abort_send;
      running_abort_recv_wakener = abort_recv_wakener;
      running_abort_send_wakener = abort_send_wakener;
      running_watch = (try_lwt
                         lwt _ = abort_recv in
                         return ()
                       with
                         | Connection_closed -> return ()
                         | exn -> fail exn);
      running_resolvers = Name_map.empty;
      running_exited_peers = OBus_cache.create 100;
      running_outgoing_m = Lwt_mutex.create ();
      running_next_serial = 1l;
      running_static_objects = Object_map.empty;
      running_dynamic_objects = Object_map.empty;
      running_incoming_filters = Lwt_sequence.create ();
      running_outgoing_filters = Lwt_sequence.create ();
      running_reply_waiters = Serial_map.empty;
      running_receiver_groups = Signal_map.empty;
      running_properties = Property_map.empty;
      running_wrapper = (connection :> t);
    } in
    connection#set_running running;
    (* Start the dispatcher *)
    ignore (dispatch_forever (connection :> t) running);
    (connection :> t)
  in
  match guid with
    | None ->
        make ()
    | Some guid ->
        match try Some(Guid_map.find guid !guid_connection_map) with Not_found -> None with
          | Some connection ->
              connection
          | None ->
              let connection = make () in
              guid_connection_map := Guid_map.add guid connection !guid_connection_map;
              connection

(* Capabilities turned on by default: *)
let capabilities = [`Unix_fd]

let of_addresses ?(shared=true) addresses = match shared with
  | false ->
      lwt guid, transport = OBus_transport.of_addresses ~capabilities addresses in
      return (of_transport transport)
  | true ->
      (* Try to find a guid that we already have *)
      let guids = OBus_util.filter_map OBus_address.guid addresses in
      match OBus_util.find_map (fun guid -> try Some(Guid_map.find guid !guid_connection_map) with Not_found -> None) guids with
        | Some packed ->
            return packed
        | None ->
            (* We ask again a shared connection even if we know that
               there is no other connection to a server with the same
               guid, because during the authentication another
               thread can add a new connection. *)
            lwt guid, transport = OBus_transport.of_addresses ~capabilities addresses in
            return (of_transport ~guid transport)

let loopback () = of_transport (OBus_transport.loopback ())

(* +-----------------------------------------------------------------+
   | Other                                                           |
   +-----------------------------------------------------------------+ *)

let running connection = connection#running

let watch connection = (running_of_connection connection).running_watch

let guid connection = (running_of_connection connection).running_guid
let transport connection = (running_of_connection connection).running_transport
let name connection = (running_of_connection connection).running_name
let support_unix_fd_passing connection =
  List.mem `Unix_fd (OBus_transport.capabilities (running_of_connection connection).running_transport)
let on_disconnect connection = (running_of_connection connection).running_on_disconnect
let close connection = match connection#get with
  | Crashed _ ->
      return ()
  | Running _ ->
      lwt _ = connection#set_crash Connection_closed in
      return ()

let state connection = (running_of_connection connection).running_state

let set_up connection =
  let running = running_of_connection connection in
  match React.S.value running.running_down with
    | None ->
        ()
    | Some(waiter, wakener) ->
        running.running_set_down None;
        wakeup wakener ()

let set_down connection =
  let running = running_of_connection connection in
  match React.S.value running.running_down with
    | Some _ ->
        ()
    | None ->
        running.running_set_down (Some(wait ()))

let incoming_filters connection = (running_of_connection connection).running_incoming_filters
let outgoing_filters connection = (running_of_connection connection).running_outgoing_filters

