(*
 * oBus_connection.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(connection)"

open Printf
open OBus_message
open OBus_private_connection
open OBus_value
open Lwt

exception Connection_closed
exception Connection_lost
exception Transport_error of exn

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

(* Apply a list of filter on a message, logging failure *)
let apply_filters typ message filters =
  try
    Lwt_sequence.fold_l
      (fun filter message -> match message with
         | Some message -> filter message
         | None -> None)
      filters (Some message)
  with exn ->
    ignore (Lwt_log.error_f ~section ~exn "an %s filter failed with" typ);
    None

(* +-----------------------------------------------------------------+
   | FDs closing                                                     |
   +-----------------------------------------------------------------+ *)

(* After a message has been dispatched, all file descriptors it
   contains are closed *)

module FD_set = Set.Make(struct type t = Unix.file_descr let compare = compare end)

let tbasic_contains_fds = function
  | Tunix_fd -> true
  | _ -> false

let rec tsingle_contains_fds = function
  | Tbasic t -> tbasic_contains_fds t
  | Tarray t -> tsingle_contains_fds t
  | Tdict(tk, tv) -> tbasic_contains_fds tk && tsingle_contains_fds tv
  | Tstructure t -> tsequence_contains_fds t
  | Tvariant -> true

and tsequence_contains_fds t = List.exists tsingle_contains_fds t

let basic_collect_fds acc = function
  | Unix_fd fd -> FD_set .add fd acc
  | _ -> acc

let rec single_collect_fds acc = function
  | Basic v ->
      basic_collect_fds acc v
  | Array(t, l) ->
      if tsingle_contains_fds t then
        List.fold_left single_collect_fds acc l
      else
        acc
  | Dict(tk, tv, l) ->
      if tbasic_contains_fds tk || tsingle_contains_fds tv then
        List.fold_left (fun acc (k, v) -> basic_collect_fds (single_collect_fds acc v) k) acc l
      else
        acc
  | Structure l ->
      sequence_collect_fds acc l
  | Variant v ->
      single_collect_fds acc v
  | Byte_array _ ->
      acc

and sequence_collect_fds acc l =
  List.fold_left single_collect_fds acc l

let close_fds message =
  FD_set.iter
    (fun fd ->
       try
         Unix.close fd
       with exn ->
         ignore (Lwt_log.error ~exn ~section "failed to close file descriptor of message"))
    (sequence_collect_fds FD_set.empty message.body)

(* +-----------------------------------------------------------------+
   | Sending messages                                                |
   +-----------------------------------------------------------------+ *)

(* Send a message, maybe adding a reply waiter and return
   [return_thread] *)
let send_message_backend connection reply_waiter_opt message =
  let running = running_of_connection connection in
  Lwt_mutex.with_lock running.running_outgoing_m begin fun () ->
    let send_it, closed = match connection#get with
      | Running _ ->
          (true, false)
      | Crashed Connection_closed ->
          (true, true)
      | Crashed _ ->
          (false, true)
    in
    if send_it then begin
      match apply_filters "outgoing" { message with serial = running.running_next_serial } running.running_outgoing_filters with
        | None ->
            lwt () = Lwt_log.debug ~section "outgoing message dropped by filters" in
            fail (Failure "message dropped by filters")

        | Some message ->
            if not closed then begin
              match reply_waiter_opt with
                | Some(waiter, wakener) ->
                    running.running_reply_waiters <- Serial_map.add message.serial wakener running.running_reply_waiters;
                    on_cancel waiter (fun () ->
                                        match connection#get with
                                          | Crashed _ ->
                                              ()
                                          | Running running ->
                                              running.running_reply_waiters <- Serial_map.remove message.serial running.running_reply_waiters)
                | None ->
                    ()
            end;

            try_lwt
              lwt () = choose [running.running_abort_send;
                               (* Do not cancel a thread while it is marshaling message: *)
                               protected (OBus_transport.send running.running_transport message)] in
              (* Everything went OK, continue with a new serial *)
              running.running_next_serial <- Int32.succ running.running_next_serial;
              return ()
            with
              | OBus_wire.Data_error _ as exn ->
                  (* The message can not be marshaled for some
                     reason. This is not a fatal error. *)
                  fail exn

              | Canceled ->
                  (* Message sending have been canceled by the
                     user. This is not a fatal error either. *)
                  fail Canceled

              | exn ->
                  (* All other errors are considered as fatal. They
                     are fatal because it is possible that a message
                     has been partially sent on the connection, so the
                     message stream is broken *)
                  lwt exn = connection#set_crash (Transport_error exn) in
                  fail exn
    end else
      match connection#get with
        | Crashed exn ->
            fail exn
        | Running _ ->
            return ()
  end

let send_message packed message =
  send_message_backend packed None message

let send_message_with_reply packed message =
  let (waiter, wakener) as v = task () in
  lwt () = send_message_backend packed (Some v) message in
  waiter

(* +-----------------------------------------------------------------+
   | Helpers for the message dispatcher                              |
   +-----------------------------------------------------------------+ *)

let send_reply connection { sender = sender; serial = serial } body =
  send_message connection { destination = sender;
                            sender = None;
                            flags = { no_reply_expected = true; no_auto_start = true };
                            serial = 0l;
                            typ = Method_return(serial);
                            body = body }

let send_error connection { sender = sender; serial = serial } name msg =
  send_message connection { destination = sender;
                            sender = None;
                            flags = { no_reply_expected = true; no_auto_start = true };
                            serial = 0l;
                            typ = Error(serial, name);
                            body = [basic(String msg)] }

let send_exn connection method_call exn =
  match OBus_error.cast exn with
    | Some(name, msg) ->
        send_error connection method_call name msg
    | None ->
        lwt () = Lwt_log.error ~section ~exn "sending an unregistred ocaml exception as a D-Bus error" in
        send_error connection method_call "ocaml.Exception" (Printexc.to_string exn)

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

let dispatch_message connection running message = match message with

  (* For method return and errors, we lookup at the reply waiters. If
     one is find then it get the reply, if none, then the reply is
     dropped. *)
  | { typ = Method_return(reply_serial) }
  | { typ = Error(reply_serial, _) } ->
      begin match try Some(Serial_map.find reply_serial running.running_reply_waiters) with Not_found -> None with
        | Some w ->
            running.running_reply_waiters <- Serial_map.remove reply_serial running.running_reply_waiters;
            wakeup w message

        | None ->
            ignore (
              Lwt_log.debug_f ~section "reply to message with serial %ld dropped%s"
                reply_serial (match message with
                                | { typ = Error(_, error_name) } ->
                                    sprintf ", the reply is the error: %S: %S"
                                      error_name
                                      (match message.body with
                                         | Basic (String x) :: _ -> x
                                         | _ -> "")
                                | _ ->
                                    "")
            )
      end

  | { typ = Signal(path, interface, member) } ->
      begin match running.running_name, message.sender with
        | None, _ ->
            (* If this is a peer-to-peer connection, we do not match
               on the sender *)
              begin match try Some(Signal_map.find (path, interface, member) running.running_receiver_groups) with Not_found -> None with
                | Some receiver_group ->
                    Lwt_sequence.iter_l
                      (fun receiver ->
                         if receiver.receiver_active && receiver.receiver_filter message then
                           try
                             receiver.receiver_push (running.running_wrapper, message)
                           with exn ->
                             ignore (Lwt_log.error ~section ~exn "signal event failed with"))
                      receiver_group.receiver_group_receivers
                | None ->
                    ()
              end

          | Some _, None ->
              ignore (Lwt_log.error_f ~section "signal without sender received from message bus")

          | Some _, Some sender ->
              begin match sender, message with

                (* Internal handling of "NameOwnerChange" messages for
                   name resolving. *)
                | "org.freedesktop.DBus",
                  { typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameOwnerChanged");
                    body = [Basic(String name); Basic(String old_owner); Basic(String new_owner)] } ->

                    let owner = if new_owner = "" then None else Some new_owner in

                    if OBus_name.is_unique name && owner = None then
                      (* If the resovler was monitoring a unique name
                         and it is not owned anymore, this means that
                         the peer with this name has exited. We
                         remember this information here. *)
                      OBus_cache.add running.running_exited_peers name;

                    begin match try Some(Name_map.find name running.running_resolvers) with Not_found -> None with
                      | Some resolver ->
                          ignore (
                            Lwt_log.debug_f ~section "updating internal name resolver: %S -> %S"
                              name
                              (match owner with
                                 | Some n -> n
                                 | None -> "")
                          );

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
                          end

                      | None ->
                          ()
                    end

                (* Internal handling of "NameAcquired" signals *)
                | ("org.freedesktop.DBus",
                   { typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameAcquired");
                     body = [Basic(String name)] })

                    (* Only handle signals destined to us *)
                    when message.destination = running.running_name ->

                    running.running_set_acquired_names (Name_set.add name (React.S.value running.running_acquired_names))

                (* Internal handling of "NameLost" signals *)
                | ("org.freedesktop.DBus",
                   { typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameLost");
                     body = [Basic(String name)] })

                    (* Only handle signals destined to us *)
                    when message.destination = running.running_name ->

                    running.running_set_acquired_names (Name_set.remove name (React.S.value running.running_acquired_names))

                | _ ->
                    ()
              end;

              (* Only handle signals broadcasted or destined to us *)
              if message.destination = None || message.destination = running.running_name then
                match try Some(Signal_map.find (path, interface, member) running.running_receiver_groups) with Not_found -> None with
                  | Some receiver_group ->
                      Lwt_sequence.iter_l
                        (fun receiver ->
                           if receiver.receiver_active && match_sender receiver message && receiver.receiver_filter message then
                             try
                               receiver.receiver_push (connection, message)
                             with exn ->
                               ignore (Lwt_log.error ~section ~exn "signal event failed with"))
                        receiver_group.receiver_group_receivers
                  | None ->
                      ()
        end

    (* Handling of the special "org.freedesktop.DBus.Peer" interface *)
    | { typ = Method_call(_, Some "org.freedesktop.DBus.Peer", member); body = body } -> begin
        match member, body with
          | "Ping", [] ->
              (* Just pong *)
              ignore (send_reply connection message [])
          | "GetMachineId", [] ->
              ignore
                (try_bind (fun _ -> Lazy.force OBus_info.machine_uuid)
                   (fun machine_uuid -> send_reply connection message [basic(string (OBus_uuid.to_string machine_uuid))])
                   (fun exn ->
                      lwt () = send_exn connection message (Failure "cannot get machine uuuid") in
                      fail exn))
          | _ ->
              ignore (send_exn connection message (unknown_method_exn message))
      end

    | { typ = Method_call(path, interface_opt, member) } ->
        ignore begin
          (* Look in static objects *)
          begin
            try
              return (Some(Object_map.find path running.running_static_objects))
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
                    return None
                | Some(path, dynamic_object) ->
                    try_lwt
                      lwt static_object = dynamic_object path in
                      return (Some static_object)
                    with exn ->
                      lwt () = Lwt_log.error ~section ~exn "dynamic object handler failed with" in
                      return None
          end >>= function
            | Some static_object ->
                ignore (try_lwt
                          static_object.static_object_handle static_object.static_object_object connection message
                        with exn ->
                          Lwt_log.error ~section ~exn "method call handler failed with");
                return ()
            | None ->
                (* Handle introspection for missing intermediate object:

                   for example if we have only one exported object with
                   path "/a/b/c", we need to add introspection support
                   for virtual objects with path "/", "/a", "/a/b",
                   "/a/b/c". *)
                if match interface_opt, member with
                  | None, "Introspect"
                  | Some "org.freedesktop.DBus.Introspectable", "Introspect" ->
                      begin match children running path with
                        | [] ->
                            true
                        | children ->
                            ignore (send_reply connection message [OBus_value.sstring (introspection children)]);
                            false
                      end
                  | _ ->
                      true
                then
                  ignore (send_exn connection message
                            (Failure (sprintf "No such object: %S" (OBus_path.to_string path))));
                return ()
        end

let read_dispatch connection running =
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
        let result = try dispatch_message connection running message; None with exn -> Some exn in
        close_fds message;
        match result with
          | None ->
              return ()
          | Some exn ->
              fail exn

let rec dispatch_forever connection running =
  try_bind
    (fun () ->
       match React.S.value running.running_down with
         | Some(waiter, wakener) ->
             lwt () = waiter in
             read_dispatch connection running
         | None ->
             read_dispatch connection running)
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

