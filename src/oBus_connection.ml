(*
 * oBus_connection.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Log = Lwt_log.Make(struct let section = "obus(connection)" end)

open Printf
open OBus_private_type
open OBus_message
open OBus_private
open OBus_value
open Lwt

exception Connection_closed
exception Connection_lost
exception Transport_error of exn

type t = OBus_private.packed_connection

let compare = Pervasives.compare

type filter = OBus_private.filter

type context = t * OBus_message.t

exception Context of context

let make_context context = Context context
let cast_context = function
  | Context context -> context
  | _ -> raise (OBus_type.Cast_failure("OBus_connection.cast_context", "context missing"))

let obus_t = Stype {
  s_type = Tnil;
  s_make = (fun _ -> Tnil);
  s_cast = (fun context l -> match context with
              | Context(connection, message) -> (connection, l)
              | _ -> raise (OBus_type.Cast_failure("OBus_connection.obus_t", "context missing")));
}

let obus_context = Stype {
  s_type = Tnil;
  s_make = (fun _ -> Tnil);
  s_cast = (fun context l -> match context with
              | Context context -> (context, l)
              | _ -> raise (OBus_type.Cast_failure("OBus_connection.obus_context", "context missing")));
}

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
    ignore (Log.exn_f exn "an %s filter failed with" typ);
    None

(* Get the error message of an error *)
let get_error msg = match msg.body with
  | Basic String x :: _ -> x
  | _ -> ""

let get packed =
  match packed#get with
    | Crashed exn ->
        raise exn
    | Running connection ->
        connection

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
  | Tvariant -> false

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
  | Byte_array _ | Variant _ ->
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
                  lwt () = Log.debug "outgoing message dropped by filters" in
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

let method_call' packed ?flags ?sender ?destination ~path ?interface ~member body ty_reply =
  lwt msg = send_message_with_reply packed
    (OBus_message.method_call ?flags ?sender ?destination ~path ?interface ~member body) in
  match msg with
    | { typ = Method_return _ } ->
        begin
          try
            return (OBus_type.cast_sequence ty_reply ~context:(Context(packed, msg)) msg.body)
          with OBus_type.Cast_failure _ as exn ->
            (* If not, check why the cast fail *)
            let expected_sig = OBus_type.type_sequence ty_reply
            and got_sig = type_of_sequence msg.body in
            if expected_sig = got_sig then
              (* If the signature match, this means that the user
                 defined a combinator raising a Cast_failure *)
              fail exn
            else
              (* In other case this means that the expected
                 signature is wrong *)
              fail
                (Failure (sprintf "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                            member (match interface with Some i -> i | None -> "")
                            (string_of_signature expected_sig)
                            (string_of_signature got_sig)))
        end

    | { typ = Error(_, error_name) } ->
        fail (OBus_error.make error_name (get_error msg))

    | _ ->
        assert false

let method_call_no_reply packed ?(flags=default_flags) ?sender ?destination ~path ?interface ~member ty =
  OBus_type.make_func ty begin fun body ->
    send_message packed (OBus_message.method_call ~flags:{ flags with no_reply_expected = true }
                               ?sender ?destination ~path ?interface ~member body)
  end

let dyn_method_call packed ?flags ?sender ?destination ~path ?interface ~member body =
  lwt { body = x } = send_message_with_reply packed
    (OBus_message.method_call ?flags ?sender ?destination ~path ?interface ~member body) in
  return x

let dyn_method_call_no_reply packed ?(flags=default_flags) ?sender ?destination ~path ?interface ~member body =
  send_message packed
    (OBus_message.method_call ~flags:{ flags with no_reply_expected = true }
       ?sender ?destination ~path ?interface ~member body)

let method_call packed ?flags ?sender ?destination ~path ?interface ~member ty =
  OBus_type.make_func ty begin fun body ->
    method_call' packed ?flags ?sender ?destination ~path ?interface ~member body (OBus_type.func_reply ty)
  end

let emit_signal packed ?flags ?sender ?destination ~path ~interface ~member ty x =
  send_message packed (OBus_message.signal ?flags ?sender ?destination ~path ~interface ~member (OBus_type.make_sequence ty x))

let dyn_emit_signal packed ?flags ?sender ?destination ~path ~interface ~member body =
  send_message packed (OBus_message.signal ?flags ?sender ?destination ~path ~interface ~member body)

let dyn_send_reply packed { sender = sender; serial = serial } body =
  send_message packed { destination = sender;
                        sender = None;
                        flags = { no_reply_expected = true; no_auto_start = true };
                        serial = 0l;
                        typ = Method_return(serial);
                        body = body }

let send_reply packed mc typ v =
  dyn_send_reply packed mc (OBus_type.make_sequence typ v)

let send_error packed { sender = sender; serial = serial } name msg =
  send_message packed { destination = sender;
                        sender = None;
                        flags = { no_reply_expected = true; no_auto_start = true };
                        serial = 0l;
                        typ = Error(serial, name);
                        body = [basic(String msg)] }

let send_exn packed method_call exn =
  match OBus_error.unmake exn with
    | Some(name, msg) ->
        send_error packed method_call name msg
    | None ->
        lwt () = Log.exn exn "sending an unregistred ocaml exception as a D-Bus error" in
        send_error packed method_call "ocaml.Exception" (Printexc.to_string exn)

let ignore_send_exn packed method_call exn = ignore(send_exn packed method_call exn)

let unknown_method packed message =
  ignore_send_exn packed message (unknown_method_exn message)

(* +-----------------------------------------------------------------+
   | Signal matching                                                 |
   +-----------------------------------------------------------------+ *)

let signal_match r = function
  | { sender = sender; typ = Signal(path, interface, member); body = body } ->
      (match r.sr_sender, sender with
         | None, _ -> true

         (* this normally never happen because with a message bus, all
            messages have a sender field *)
         | _, None -> false

         | Some name, Some sender -> match React.S.value name with
             | None ->
                 (* This case is when the name the rule filter on do
                    not currently have an owner *)
                 false

             | Some owner -> owner = sender) &&
        (r.sr_path = path) &&
        (r.sr_interface = interface) &&
        (r.sr_member = member)

  | _ ->
      false

let signal_match_ignore_sender r = function
  | { typ = Signal(path, interface, member); body = body } ->
      (r.sr_path = path) &&
        (r.sr_interface = interface) &&
        (r.sr_member = member)

  | _ ->
      false

(* +-----------------------------------------------------------------+
   | Reading/dispatching                                             |
   +-----------------------------------------------------------------+ *)

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
              Log.debug_f "reply to message with serial %ld dropped%s"
                reply_serial (match message with
                                | { typ = Error(_, error_name) } ->
                                    sprintf ", the reply is the error: %S: %S"
                                      error_name (get_error message)
                                | _ ->
                                  "")
            )
      end

  | { typ = Signal _ } ->
      begin match connection.name, message.sender with
        | None, _
        | _, None ->
            (* If this is a peer-to-peer connection, we do match on
               the sender *)
            Lwt_sequence.iter_l
              (fun receiver ->
                 if signal_match_ignore_sender receiver message then
                   try
                     receiver.sr_push (connection.packed, message)
                   with exn ->
                     ignore (Log.exn exn "signal event failed with"))
              connection.signal_receivers

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
                          Log.debug "updating internal name resolver: %S -> %S"
                            name
                            (match owner with
                               | Some n -> n
                               | None -> "")
                        );

                        nr.nr_set owner;

                        if not nr.nr_init_done then begin
                          (* The resolver has not yet been
                             initialized; this means that the reply to
                             GetNameOwner (done by
                             [OBus_resolver.make]) has not yet been
                             received. We consider that this first
                             signal has precedence and terminate
                             initialization. *)
                          nr.nr_init_done <- true;
                          Lwt.wakeup nr.nr_init_wakener ()
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

                  connection.acquired_names <- name :: connection.acquired_names

              (* Internal handling of "NameLost" signals *)
              | ("org.freedesktop.DBus",
                 { typ = Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameLost");
                   body = [Basic(String name)] })

                  (* Only handle signals destined to us *)
                  when message.destination = connection.name ->

                  connection.acquired_names <- List.filter ((<>) name) connection.acquired_names

              | _ ->
                  ()
            end;

            (* Only handle signals broadcasted or destined to us *)
            if message.destination = None || message.destination = connection.name then
              Lwt_sequence.iter_l
                (fun receiver ->
                   if signal_match receiver message then
                     try
                       receiver.sr_push (connection.packed, message)
                     with exn ->
                       ignore (Log.exn exn "signal event failed with"))
                connection.signal_receivers
      end

  (* Handling of the special "org.freedesktop.DBus.Peer" interface *)
  | { typ = Method_call(_, Some "org.freedesktop.DBus.Peer", member); body = body } -> begin
      match member, body with
        | "Ping", [] ->
            (* Just pong *)
            ignore (dyn_send_reply connection.packed message [])
        | "GetMachineId", [] ->
            ignore
              (try_bind (fun _ -> Lazy.force OBus_info.machine_uuid)
                 (fun machine_uuid -> dyn_send_reply connection.packed message [basic(string (OBus_uuid.to_string machine_uuid))])
                 (fun exn ->
                    lwt () = send_exn connection.packed message (Failure "cannot get machine uuuid") in
                    fail exn))
        | _ ->
            unknown_method connection.packed message
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
                    lwt () = Log.exn exn "dynamic object handler failed with" in
                    return None
          else
            return obj
        in
        match obj with
          | Some obj ->
              begin try
                obj.oo_handle obj.oo_object connection.packed message
              with exn ->
                ignore (Log.exn exn "method call handler failed with")
              end;
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
                          ignore
                            (send_reply connection.packed message <:obus_type< OBus_introspect.document >>
                               ([("org.freedesktop.DBus.Introspectable",
                                  [OBus_introspect.Method("Introspect", [],
                                                          [(None, Tbasic Tstring)], [])],
                                  [])], l));
                          false
                    end
                | _ ->
                    true
              then
                ignore_send_exn connection.packed message
                  (Failure (sprintf "No such object: %S" (OBus_path.to_string path)));
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
        lwt () = Log.debug "incoming message dropped by filters" in
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
             Log.exn exn "the error handler (OBus_connection.on_disconnect) failed with")

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
              Log.exn exn "failed to abort/shutdown the transport"
        in
        return exn
end

(* +-----------------------------------------------------------------+
   | Connection creation                                             |
   +-----------------------------------------------------------------+ *)

let of_transport ?guid ?(up=true) transport =
  let make _ =
    let abort_recv, abort_recv_wakener = Lwt.wait ()
    and abort_send, abort_send_wakener = Lwt.wait ()
    and packed_connection = new packed_connection
    and down, set_down = React.S.create (if up then None else Some(wait ())) in
    let state = React.S.map (function None -> `Up | Some _ -> `Down) down in
    let connection = {
      name = None;
      acquired_names = [];
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
      signal_receivers = Lwt_sequence.create ();
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

let watch packed = (get packed).watch

let guid packed = (get packed).guid
let transport packed = (get packed).transport
let name packed = (get packed).name
let support_unix_fd_passing packed =
  List.mem `Unix_fd (OBus_transport.capabilities (get packed).transport)
let on_disconnect packed = (get packed).on_disconnect
let close packed = match packed#get with
  | Crashed _ ->
      return ()
  | Running _ ->
      lwt _ = packed#set_crash Connection_closed in
      return ()

let state packed = (get packed).state

let set_up packed =
  let connection = get packed in
  match React.S.value connection.down with
    | None ->
        ()
    | Some(waiter, wakener) ->
        connection.set_down None;
        wakeup wakener ()

let set_down packed =
  let connection = get packed in
  match React.S.value connection.down with
    | Some _ ->
        ()
    | None ->
        connection.set_down (Some(wait ()))

let incoming_filters packed = (get packed).incoming_filters
let outgoing_filters packed = (get packed).outgoing_filters

