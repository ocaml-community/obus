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
open OBus_private
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

type t = OBus_private.packed_connection

let compare = Pervasives.compare

type filter = OBus_private.filter

(* Mapping from server guid to connection. *)
module GuidMap = OBus_util.MakeMap(struct
                                     type t = OBus_address.guid
                                     let compare = Pervasives.compare
                                   end)
let guid_connection_map = ref GuidMap.empty

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

let basic_close_fds = function
  | Unix_fd fd -> Unix.close fd
  | _ -> ()

let rec single_close_fds = function
  | Basic v ->
      basic_close_fds v
  | Array(t, l) ->
      if tsingle_contains_fds t then
        List.iter single_close_fds l
      else
        ()
  | Dict(tk, tv, l) ->
      if tbasic_contains_fds tk || tsingle_contains_fds tv then
        List.iter (fun (k, v) -> basic_close_fds k; single_close_fds v) l
  | Structure l ->
      sequence_close_fds l
  | Variant v ->
      single_close_fds v
  | Byte_array _ ->
      ()

and sequence_close_fds l =
  List.iter single_close_fds l

(* +-----------------------------------------------------------------+
   | Sending messages                                                |
   +-----------------------------------------------------------------+ *)

(* Send a message, maybe adding a reply waiter and return
   [return_thread] *)
let send_message_backend packed reply_waiter_opt message =
  match packed#get with
    | Crashed exn ->
        fail exn
    | Running connection ->
        Lwt_mutex.with_lock connection.outgoing_m begin fun () ->
          let send_it, closed = match packed#get with
            | Running _ ->
                (true, false)
            | Crashed Connection_closed ->
                (true, true)
            | Crashed _ ->
                (false, true)
          in
          if send_it then begin
            match apply_filters "outgoing" { message with serial = connection.next_serial } connection.outgoing_filters with
              | None ->
                  lwt () = Lwt_log.debug ~section "outgoing message dropped by filters" in
                  fail (Failure "message dropped by filters")

              | Some message ->
                  if not closed then begin
                    match reply_waiter_opt with
                      | Some(waiter, wakener) ->
                          connection.reply_waiters <- SerialMap.add message.serial wakener connection.reply_waiters;
                          on_cancel waiter (fun () ->
                                              match packed#get with
                                                | Crashed _ ->
                                                    ()
                                                | Running connection ->
                                                    connection.reply_waiters <- SerialMap.remove message.serial connection.reply_waiters)
                      | None ->
                          ()
                  end;

                  try_lwt
                    lwt () = choose [connection.abort_send;
                                     (* Do not cancel a thread while it is marshaling message: *)
                                     protected (OBus_transport.send connection.transport message)] in
                    (* Everything went OK, continue with a new serial *)
                    connection.next_serial <- Int32.succ connection.next_serial;
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
                        (* All other errors are considered as
                           fatal. They are fatal because it is possible
                           that a message has been partially sent on the
                           connection, so the message stream is
                           broken *)
                        lwt exn = connection.packed#set_crash (Transport_error exn) in
                        fail exn
          end else
            match packed#get with
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

let send_reply packed { sender = sender; serial = serial } body =
  send_message packed { destination = sender;
                        sender = None;
                        flags = { no_reply_expected = true; no_auto_start = true };
                        serial = 0l;
                        typ = Method_return(serial);
                        body = body }

let send_error packed { sender = sender; serial = serial } name msg =
  send_message packed { destination = sender;
                        sender = None;
                        flags = { no_reply_expected = true; no_auto_start = true };
                        serial = 0l;
                        typ = Error(serial, name);
                        body = [basic(String msg)] }

let send_exn packed method_call exn =
  match OBus_error.cast exn with
    | Some(name, msg) ->
        send_error packed method_call name msg
    | None ->
        lwt () = Lwt_log.error ~section ~exn "sending an unregistred ocaml exception as a D-Bus error" in
        send_error packed method_call "ocaml.Exception" (Printexc.to_string exn)

(* +-----------------------------------------------------------------+
   | Signal matching                                                 |
   +-----------------------------------------------------------------+ *)

let match_sender signal_receiver message =
  match signal_receiver.sr_sender, message.sender with
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

let introspectable = "\
<node>
  <interface name=\"org.freedesktop.DBus.Introspectable\">
    <method name=\"Introspect\">
      <arg name=\"data\" direction=\"out\" type=\"s\"/>
    </method>
  </interface>
</node>
"

let dispatch_message connection message = match message with

  (* For method return and errors, we lookup at the reply waiters. If
     one is find then it get the reply, if none, then the reply is
     dropped. *)
  | { typ = Method_return(reply_serial) }
  | { typ = Error(reply_serial, _) } ->
      begin match SerialMap.lookup reply_serial connection.reply_waiters with
        | Some w ->
            connection.reply_waiters <- SerialMap.remove reply_serial connection.reply_waiters;
            wakeup w message

        | None ->
            ignore (
              Lwt_log.debug_f ~section "reply to message with serial %ld dropped%s"
                reply_serial (match message with
                                | { typ = Error(_, error_name) } ->
                                    sprintf ", the reply is the error: %S: %S"
                                      error_name
                                      (match msg.body with
                                         | Basic (String x) :: _ -> x
                                         | _ -> "")
                                | _ ->
                                    "")
            )
      end

  | { typ = Signal(path, interface, member) } ->
      begin match connection.name, message.sender with
        | None, _ ->
            (* If this is a peer-to-peer connection, we do not match
               on the sender *)
            begin match SignalMap.lookup (path, interface, member) connection.signal_receivers with
              | Some set ->
                  Lwt_sequence.iter_l
                    (fun receiver ->
                       if receiver.sr_active then
                         try
                           receiver.sr_push (connection.packed, message)
                         with exn ->
                           ignore (Lwt_log.error ~section ~exn "signal event failed with"))
                    set.srs_receivers
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
                       the peer with this name has exited. We remember
                       this information here. *)
                    OBus_cache.add connection.exited_peers name;

                  begin match NameMap.lookup name connection.name_resolvers with
                    | Some nr ->
                        ignore (
                          Lwt_log.debug_f ~section "updating internal name resolver: %S -> %S"
                            name
                            (match owner with
                               | Some n -> n
                               | None -> "")
                        );

                        nr.nr_set owner;

                        begin
                          match nr.nr_state with
                            | Nrs_init(waiter, wakener) ->
                                (* The resolver has not yet been
                                   initialized; this means that the
                                   reply to GetNameOwner (done by
                                   [OBus_resolver.make]) has not yet
                                   been received. We consider that
                                   this first signal has precedence
                                   and terminate initialization. *)
                                nr.nr_state <- Nrs_running;
                                Lwt.wakeup wakener nr
                            | Nrs_running ->
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
                  when message.destination = connection.name ->

                  connection.set_acquired_names (NameSet.add name (React.S.value connection.acquired_names))

              (* Internal handling of "NameLost" signals *)
              | ("org.freedesktop.DBus",
                 { typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameLost");
                   body = [Basic(String name)] })

                  (* Only handle signals destined to us *)
                  when message.destination = connection.name ->

                  connection.set_acquired_names (NameSet.remove name (React.S.value connection.acquired_names))

              | _ ->
                  ()
            end;

            (* Only handle signals broadcasted or destined to us *)
            if message.destination = None || message.destination = connection.name then
              match SignalMap.lookup (path, interface, member) connection.signal_receivers with
                | Some set ->
                    Lwt_sequence.iter_l
                      (fun receiver ->
                         if receiver.sr_active && match_sender receiver message then
                           try
                             receiver.sr_push (connection.packed, message)
                           with exn ->
                             ignore (Lwt_log.error ~section ~exn "signal event failed with"))
                      set.srs_receivers
                | None ->
                    ()
      end

  (* Handling of the special "org.freedesktop.DBus.Peer" interface *)
  | { typ = Method_call(_, Some "org.freedesktop.DBus.Peer", member); body = body } -> begin
      match member, body with
        | "Ping", [] ->
            (* Just pong *)
            ignore (send_reply connection.packed message [])
        | "GetMachineId", [] ->
            ignore
              (try_bind (fun _ -> Lazy.force OBus_info.machine_uuid)
                 (fun machine_uuid -> send_reply connection.packed message [basic(string (OBus_uuid.to_string machine_uuid))])
                 (fun exn ->
                    lwt () = send_exn connection.packed message (Failure "cannot get machine uuuid") in
                    fail exn))
        | _ ->
            ignore (send_exn connection.packed message (unknown_method_exn message))
    end

  | { typ = Method_call(path, interface_opt, member) } ->
      ignore begin
        (* Look in static objects *)
        let obj = ObjectMap.lookup path connection.exported_objects in
        lwt obj =
          if obj = None then
            (* Look in dynamic objects *)
            match
              OBus_util.find_map
                (fun dynobj ->
                   match OBus_path.after dynobj.do_prefix path with
                     | Some path ->
                         Some(path, dynobj)
                     | None ->
                         None)
                connection.dynamic_objects
            with
              | None ->
                  return None
              | Some(path, dynobj) ->
                  try_lwt
                    lwt obj = dynobj.do_create path in
                    return (Some obj)
                  with exn ->
                    lwt () = Lwt_log.error ~section ~exn "dynamic object handler failed with" in
                    return None
          else
            return obj
        in
        match obj with
          | Some obj ->
              ignore (try_lwt
                        obj.oo_handle obj.oo_object connection.packed message
                      with exn ->
                        Lwt_log.error ~section ~exn "method call handler failed with");
              return ()
          | None ->
              (* Handle introspection for missing intermediate object:

                 for example if we have only one exported object with
                 path "/a/b/c", we need to add introspection support for
                 virtual objects with path "/", "/a", "/a/b",
                 "/a/b/c". *)
              if match interface_opt, member with
                | None, "Introspect"
                | Some "org.freedesktop.DBus.Introspectable", "Introspect" ->
                    begin match children connection path with
                      | [] ->
                          true
                      | l ->
                          ignore (send_reply connection.packed message [OBus_value.sstring introspectable]);
                          false
                    end
                | _ ->
                    true
              then
                ignore (send_exn connection.packed message
                          (Failure (sprintf "No such object: %S" (OBus_path.to_string path))));
              return ()
      end

let read_dispatch connection =
  lwt message =
    try_lwt
      choose [OBus_transport.recv connection.transport;
              connection.abort_recv]
    with exn ->
      connection.packed#set_crash
        (match exn with
           | End_of_file -> Connection_lost
           | OBus_wire.Protocol_error _ as exn -> exn
           | exn -> Transport_error exn) >>= fail
  in
  match apply_filters "incoming" message connection.incoming_filters with
    | None ->
        lwt () = Lwt_log.debug ~section "incoming message dropped by filters" in
        return ()
    | Some message ->
        dispatch_message connection message;
        sequence_close_fds (OBus_message.body message);
        return ()

let rec dispatch_forever connection =
  try_bind
    (fun () ->
       match React.S.value connection.down with
         | Some(waiter, wakener) ->
             lwt () = waiter in
             read_dispatch connection
         | None ->
             read_dispatch connection)
    (fun () ->
       dispatch_forever connection)
    (function
       | Connection_closed ->
           return ()
       | exn ->
           (* At this state of the program the connection state should
              have been already set to [Crashed].

              The only possible error maybe an [Out_of_memory].
           *)
           lwt exn = connection.packed#set_crash exn in
           try
             !(connection.on_disconnect) exn;
             return ()
           with exn ->
             Lwt_log.error ~section ~exn "the error handler (OBus_connection.on_disconnect) failed with")

(* +-----------------------------------------------------------------+
   | ``Packed'' connection                                           |
   +-----------------------------------------------------------------+ *)

class packed_connection =
  let running, set_running = React.S.create true in
object(self)

  method running = running

  val mutable state = Crashed Exit (* Fake initial state *)

  (* Set the initial running state *)
  method set_connection connection =
    state <- Running connection

  method get = state

  (* Put the connection in a "crashed" state. This means that all
     subsequent call using the connection will fail. *)
  method set_crash exn = match state with
    | Crashed exn ->
        return exn
    | Running connection ->
        state <- Crashed exn;
        set_running false;

        begin match connection.guid with
          | Some guid -> guid_connection_map := GuidMap.remove guid !guid_connection_map
          | None -> ()
        end;

        (* This make the dispatcher to exit if it is waiting on
           [get_message] *)
        wakeup_exn connection.abort_recv_wakener exn;
        begin match React.S.value connection.down with
          | Some(waiter, wakener) -> wakeup_exn wakener exn
          | None -> ()
        end;

        (* Wakeup all reply handlers so they will not wait forever *)
        SerialMap.iter (fun _ w -> wakeup_exn w exn) connection.reply_waiters;

        (* Remove all objects *)
        ObjectMap.iter (fun path obj -> obj.oo_connection_closed connection.packed) connection.exported_objects;

        (* If the connection is closed normally, flush it *)
        lwt () =
          if exn = Connection_closed then
            Lwt_mutex.with_lock connection.outgoing_m return
          else begin
            wakeup_exn connection.abort_send_wakener exn;
            return ()
          end
        in

        (* Shutdown the transport *)
        lwt () =
            try_lwt
              OBus_transport.shutdown connection.transport
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
    and packed_connection = new packed_connection
    and down, set_down = React.S.create (if up then None else Some(wait ()))
    and acquired_names, set_acquired_names = React.S.create ~eq:NameSet.equal NameSet.empty in
    let state = React.S.map (function None -> `Up | Some _ -> `Down) down in
    let connection = {
      name = None;
      acquired_names = acquired_names;
      set_acquired_names = set_acquired_names;
      transport = transport;
      on_disconnect = ref (fun _ -> ());
      guid = guid;
      down = down;
      set_down = set_down;
      state = state;
      abort_recv = abort_recv;
      abort_send = abort_send;
      abort_recv_wakener = abort_recv_wakener;
      abort_send_wakener = abort_send_wakener;
      watch = (try_lwt
                 lwt _ = abort_recv in
                 return ()
               with
                 | Connection_closed -> return ()
                 | exn -> fail exn);
      name_resolvers = NameMap.empty;
      exited_peers = OBus_cache.create 100;
      outgoing_m = Lwt_mutex.create ();
      next_serial = 1l;
      exported_objects = ObjectMap.empty;
      dynamic_objects = [];
      incoming_filters = Lwt_sequence.create ();
      outgoing_filters = Lwt_sequence.create ();
      reply_waiters = SerialMap.empty;
      signal_receivers = SignalMap.empty;
      properties = PropertyMap.empty;
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
        match GuidMap.lookup guid !guid_connection_map with
          | Some connection -> connection
          | None ->
              let connection = make () in
              guid_connection_map := GuidMap.add guid connection !guid_connection_map;
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
      match OBus_util.find_map (fun guid -> GuidMap.lookup guid !guid_connection_map) guids with
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

let running packed = packed#running

let watch packed = (unpack_connection packed).watch

let guid packed = (unpack_connection packed).guid
let transport packed = (unpack_connection packed).transport
let name packed = (unpack_connection packed).name
let support_unix_fd_passing packed =
  List.mem `Unix_fd (OBus_transport.capabilities (unpack_connection packed).transport)
let on_disconnect packed = (unpack_connection packed).on_disconnect
let close packed = match packed#get with
  | Crashed _ ->
      return ()
  | Running _ ->
      lwt _ = packed#set_crash Connection_closed in
      return ()

let state packed = (unpack_connection packed).state

let set_up packed =
  let connection = unpack_connection packed in
  match React.S.value connection.down with
    | None ->
        ()
    | Some(waiter, wakener) ->
        connection.set_down None;
        wakeup wakener ()

let set_down packed =
  let connection = unpack_connection packed in
  match React.S.value connection.down with
    | Some _ ->
        ()
    | None ->
        connection.set_down (Some(wait ()))

let incoming_filters packed = (unpack_connection packed).incoming_filters
let outgoing_filters packed = (unpack_connection packed).outgoing_filters

