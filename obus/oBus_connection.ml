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

type t = connection

type filter = OBus_internals.filter
type filter_id = filter MSet.node

module Serial_map = Util.Make_map(struct type t = serial end)
module Object_map = Util.Make_map(struct type t = OBus_path.t end)
module Name_map = Util.Make_map(struct type t = OBus_name.bus end)

exception Context of connection * OBus_message.any
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

(* +------------------------------+
   | Helpers for sending messages |
   +------------------------------+ *)

let send_message (connection : connection) message = connection#send_message message
let send_message_with_reply (connection : connection) message = connection#send_message_with_reply message

let method_call' connection ?flags ?sender ?destination ~path ?interface ~member body ty_reply =
  send_message_with_reply connection (method_call ?flags ?sender ?destination ~path ?interface ~member body)
  >>= fun msg ->
    try
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
            (* In other case this means that the expected signature is
               wrong *)
            fail
              (Failure (sprintf "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                          member (match interface with Some i -> i | None -> "")
                          (string_of_signature expected_sig)
                          (string_of_signature got_sig)))

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

(* +------------+
   | Connection |
   +------------+ *)

class connection ?guid ?(initially_up=true) transport =
  let abort = Lwt.wait () in
  (* [abort] is a waiting thread which is wakeup when the connection
     is closed or aborted. It is used to make the dispatcher to
     exit. *)
object(self)

  val watch =
    try_bind (fun _ -> abort)
      (fun _ -> return ())
      (function
         | Connection_closed -> return ()
         | exn -> fail exn)

  method watch = watch

  val mutable crashed = None
    (* Set when the connection has crashed or has been closed, all
       functions will fail with this exception if it is set *)

  method running = crashed = None

  (* All the following public methods must check that the connection
     is still running before anything else *)

  (* +---------------------+
     | Exported parameters |
     +---------------------+ *)

  method guid = match crashed with
    | Some exn -> raise exn
    | None -> guid

  method transport = match crashed with
    | Some exn -> raise exn
    | None -> transport

  val mutable name : OBus_name.bus option = None
    (* Unique name of the connection. If set this means that the other
       side is a message bus. *)

  method is_bus = match crashed with
    | Some exn -> raise exn
    | None -> name <> None

  method name = match crashed with
    | Some exn -> raise exn
    | None -> name

  method set_name n = match crashed with
    | Some exn -> raise exn
    | None -> name <- Some n

  val mutable acquired_names : OBus_name.bus list = []
    (* List of names we currently own *)

  method acquired_names = match crashed with
    | Some exn -> raise exn
    | None -> acquired_names

  val on_disconnect : (exn -> unit) ref = ref (fun _ -> ())
    (* [on_disconnect] is called when the connection is
       disconnect. This can happen is receiving a message on the
       transport fail, or if a failure happen while a message is being
       sent. *)

  method on_disconnect = match crashed with
    | Some exn -> raise exn
    | None -> on_disconnect

  val shutdown_transport_on_close = ref true
    (* This tell weather we must shutdown the transport when the
       connection is closed by the programmer or by a crash *)

  method shutdown_transport_on_close = match crashed with
    | Some exn -> raise exn
    | None -> shutdown_transport_on_close

  method close = match crashed with
    | Some exn -> raise exn
    | None -> ignore (self#set_crash Connection_closed)

  (* +---------------+
     | Up/down state |
     +---------------+ *)

  (* up/down state. If [None] it means that the connection is up, if
     [Some w] it means that the connection is down, [w] being a
     waiting thread which will be wake up when the connection is set
     up *)
  val mutable down = match initially_up with
    | true -> None
    | false -> Some(wait ())

  method is_up = match crashed with
    | Some exn -> raise exn
    | None -> down = None

  method set_up = match crashed with
    | Some exn -> raise exn
    | None -> match down with
        | None -> ()
        | Some w ->
            down <- None;
            wakeup w ()

  method set_down = match crashed with
    | Some exn -> raise exn
    | None -> match down with
        | Some _ -> ()
        | None -> down <- Some(wait ())

  (* +---------+
     | Filters |
     +---------+ *)

  val incoming_filters : filter MSet.t = MSet.make ()
  val outgoing_filters : filter MSet.t = MSet.make ()

  method add_incoming_filter filter = match crashed with
    | Some exn -> raise exn
    | None -> MSet.add incoming_filters filter

  method add_outgoing_filter filter = match crashed with
    | Some exn -> raise exn
    | None -> MSet.add outgoing_filters filter

  (* +------------------+
     | Sending messages |
     +------------------+ *)

  val mutable outgoing = Lwt.return 1l
    (* The ougoing thread.

       If a message is being sent, it is a waiting thread which is
       wakeup when the message is sent. It return the current
       serial. *)

  val mutable reply_waiters : OBus_message.method_return Lwt.t Serial_map.t = Serial_map.empty
    (* Mapping serial -> thread waiting for a reply *)

  (* Send a message, maybe adding a reply waiter and return
     [return_thread] *)
  method private send_message_backend : 'a 'b. OBus_message.method_return Lwt.t option -> 'a Lwt.t -> ([< OBus_message.any_type ] as 'b) OBus_message.t -> 'a Lwt.t =
    fun reply_waiter_opt return_thread message ->
      let current_outgoing = outgoing in
      let w = wait () in
      outgoing <- w;
      current_outgoing >>= fun serial ->
        match apply_filters "outgoing" { (message :> any) with serial = serial } outgoing_filters with
          | None ->
              Log.debug "outgoing message dropped by filters";
              wakeup w serial;
              fail (Failure "message dropped by filters")

          | Some message ->
              begin match reply_waiter_opt with
                | Some w ->
                    reply_waiters <- Serial_map.add serial w reply_waiters
                | None ->
                    ()
              end;

              if !(OBus_info.dump) then
                Format.eprintf "-----@\n@[<hv 2>sending message:@\n%a@]@."
                  OBus_message.print message;

              try_bind
                (fun _ -> OBus_lowlevel.send transport message)
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
                       let exn = self#set_crash (Transport_error exn) in
                       wakeup_exn w exn;
                       fail exn)

  method send_message : 'a. ([< OBus_message.any_type ] as 'a) OBus_message.t -> unit Lwt.t =
    fun message -> match crashed with
      | Some exn ->
          fail exn
      | None ->
          self#send_message_backend None (return ()) message

  method send_message_with_reply (message : OBus_message.method_call) = match crashed with
    | Some exn ->
        fail exn
    | None ->
        let w = wait () in
        self#send_message_backend (Some w) w (message :> OBus_message.any)

  (* +----------------+
     | Name resolvers |
     +----------------+ *)

  val mutable name_resolvers = Name_map.empty
    (* Mapping bus-name <-> resolver *)

  method add_name_resolver name nr = match crashed with
    | Some exn -> raise exn
    | None -> name_resolvers <- Name_map.add name nr name_resolvers

  method remove_name_resolver name = match crashed with
    | Some exn -> raise exn
    | None -> name_resolvers <- Name_map.remove name name_resolvers

  method find_name_resolver name = match crashed with
    | Some exn -> raise exn
    | None -> Name_map.lookup name name_resolvers

  val exited_peers = Cache.create 100
    (* Cache of bus names of peer which has exited. It is used by
       [OBus_resolver] to minimize the number of request to the
       message bus *)

  method peer_has_exited name = match crashed with
    | Some exn -> raise exn
    | None -> Cache.mem exited_peers name

  method add_exited_peer name = match crashed with
    | Some exn -> raise exn
    | None -> Cache.add exited_peers name

  (* +---------------------+
     | Reading/dispatching |
     +---------------------+ *)

  val signal_receivers = MSet.make ()

  method add_signal_receiver sr = match crashed with
    | Some exn -> raise exn
    | None -> MSet.add signal_receivers sr

  val mutable exported_objects : dbus_object Object_map.t = Object_map.empty

  method export_object path obj = match crashed with
    | Some exn -> raise exn
    | None -> exported_objects <- Object_map.add path obj exported_objects

  method remove_object path = match crashed with
    | Some exn -> raise exn
    | None -> exported_objects <- Object_map.remove path exported_objects

  method find_object path = match crashed with
    | Some exn -> raise exn
    | None -> Object_map.lookup path exported_objects

  (* Find the handler for a reply and remove it. *)
  method private find_reply_waiter serial f g =
    match Serial_map.lookup serial reply_waiters with
      | Some x ->
          reply_waiters <- Serial_map.remove serial reply_waiters;
          f x
      | None ->
          g ()

  method children path = match crashed with
    | Some exn ->
        raise exn

    | None ->
        Object_map.fold
          (fun p obj acc -> match OBus_path.after path p with
             | Some(elt :: _) -> if List.mem elt acc then acc else elt :: acc
             | _ -> acc)
          exported_objects []

  method private dispatch_message = function
      (* For method return and errors, we lookup at the reply
         waiters. If one is find then it get the reply, if none, then
         the reply is dropped. *)
    | { typ = `Method_return(reply_serial) } as message ->
        self#find_reply_waiter reply_serial
          (fun w -> wakeup w message)
          (fun _ -> Log.debug "reply to message with serial %ld dropped" reply_serial);

    | { typ = `Error(reply_serial, error_name) } as message ->
        let msg = get_error message in
        self#find_reply_waiter reply_serial
          (fun w -> wakeup_exn w (OBus_error.make error_name msg))
          (fun _ ->
             Log.debug "error reply to message with serial %ld dropped because no reply was expected, \
                        the error is: %S: %S" reply_serial error_name msg)

    | { typ = `Signal _ } as message ->
        begin match name, message.sender with
          | None, _
          | _, None ->
              (* If this is a peer-to-peer connection, we do match on
                 the sender *)
              MSet.iter
                (fun receiver ->
                   if signal_match_ignore_sender receiver message
                   then callback_apply "signal callback" receiver.sr_callback ((self :> connection), message))
                signal_receivers

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
                         the peer with this name has exited. We
                         remember this information here. *)
                      Cache.add exited_peers name;

                    begin match Name_map.lookup name name_resolvers with
                      | Some nr ->
                          Log.debug "updating internal name resolver: %S -> %S" name (match owner with
                                                                                        | Some n -> n
                                                                                        | None -> "");
                          nr.nr_owner := owner;

                          if not nr.nr_initialized then begin
                            (* The resolver has not yet been
                               initialized; this means that the reply
                               to GetNameOwner (done by
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
                | "org.freedesktop.DBus",
                    { typ = `Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameAcquired");
                      body = [Basic(String name')] }

                      (* Only handle signals destined to us *)
                      when message.destination = name ->

                    acquired_names <- name' :: acquired_names

                (* Internal handling of "NameLost" signals *)
                | "org.freedesktop.DBus",
                    { typ = `Signal(["org"; "freedesktop"; "DBus"], "org.freedesktop.DBus", "NameAcquired");
                      body = [Basic(String name')] }

                      (* Only handle signals destined to us *)
                      when message.destination = name ->

                    acquired_names <- List.filter ((<>) name') acquired_names

                | _ ->
                    ()
              end;

              (* Only handle signals broadcasted destined to us *)
              if message.destination = None || message.destination = name then
                MSet.iter
                  (fun receiver ->
                     if signal_match receiver message
                     then callback_apply "signal callback" receiver.sr_callback ((self :> connection), message))
                  signal_receivers
        end

    (* Handling of the special "org.freedesktop.DBus.Peer" interface *)
    | { typ = `Method_call(_, Some "org.freedesktop.DBus.Peer", member); body = body } as message -> begin
        match member, body with
          | "Ping", [] ->
              (* Just pong *)
              ignore (dsend_reply (self :> connection) message [])
          | "GetMachineId", [] ->
              ignore
                (try_bind (fun _ -> Lazy.force OBus_info.machine_uuid)
                   (fun machine_uuid -> send_reply (self :> connection) message <:obus_type< string >> (OBus_uuid.to_string machine_uuid))
                   (fun exn -> perform
                      send_exn (self :> connection) message (OBus_error.Failed "cannot get machine uuuid");
                      fail exn))
          | _ ->
              unknown_method (self :> connection) message
      end

    | { typ = `Method_call(path, interface_opt, member) } as message ->
        match Object_map.lookup path exported_objects with
          | Some obj ->
              begin try
                obj#obus_handle_call (self :> connection) message
              with
                  exn ->
                    Log.failure exn "method call handler failed with"
              end
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
                      begin match self#children path with
                        | [] -> false
                        | l ->
                            ignore
                              (send_reply (self :> connection) message <:obus_type< OBus_introspect.document >>
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
                    ignore_send_exn (self :> connection) message
                      (OBus_error.Failed (sprintf "No such object: %S" (OBus_path.to_string path)))

  method private read_dispatch =
    catch
      (fun _ -> choose [OBus_lowlevel.recv transport;
                        abort])
      (fun exn ->
         fail (self#set_crash
                 (match exn with
                    | End_of_file -> Connection_lost
                    | OBus_lowlevel.Protocol_error _ as exn -> exn
                    | exn -> Transport_error exn)))
    >>= fun message ->

      if !(OBus_info.dump) then
        Format.eprintf "-----@\n@[<hv 2>message received:@\n%a@]@."
          OBus_message.print message;

      match apply_filters "incoming" message incoming_filters with
        | None ->
            Log.debug "incoming message dropped by filters";
            return ()
        | Some message ->
            self#dispatch_message message;
            return ()

  method private dispatch_forever =
    try_bind
      (fun _ -> match down with
         | Some w -> w >>= (fun _ -> self#read_dispatch)
         | None -> self#read_dispatch)
      (fun _ -> self#dispatch_forever)
      (function
         | Connection_closed -> return ()
         | exn ->
             try
               !on_disconnect exn;
               return ()
             with
                 exn ->
                   Log.failure exn "the error handler (OBus_connection.on_disconnect) failed with:";
                   return ())

  (* +----------------+
     | Error handling |
     +----------------+ *)

  (* Put the connection in a "crashed" state. This means that all
     subsequent call using the connection will fail. *)
  method private set_crash exn = match crashed with
    | Some exn -> exn
    | None ->
        crashed <- Some exn;
        begin match guid with
          | Some guid -> guid_connection_map := Guid_map.remove guid !guid_connection_map
          | None -> ()
        end;

        MSet.clear signal_receivers;
        MSet.clear incoming_filters;
        MSet.clear outgoing_filters;
        name_resolvers <- Name_map.empty;
        Cache.clear exited_peers;

        (* This make the dispatcher to exit if it is waiting on
           [get_message] *)
        wakeup_exn abort exn;
        begin match down with
          | Some w -> wakeup_exn w exn
          | None -> ()
        end;

        (* Shutdown the transport *)
        if !shutdown_transport_on_close then
          (try
             OBus_lowlevel.shutdown transport
           with
               exn ->
                 Log.failure exn "failed to abort/shutdown the transport");

        (* Wakeup all reply handlers so they will not wait forever *)
        Serial_map.iter (fun _ w -> wakeup_exn w exn) reply_waiters;

        (* Remove all objects *)
        Object_map.iter begin fun p obj ->
          try
            obj#obus_connection_closed (self :> connection)
          with
              exn ->
                (* This may happen if the programmer has overridden
                   the method *)
                Log.failure exn "obus_connection_closed on object with path %S failed with"
                  (OBus_path.to_string p)
        end exported_objects;

        reply_waiters <- Serial_map.empty;
        exported_objects <- Object_map.empty;

        exn

  initializer
    ignore (self#dispatch_forever)
end

let is_up connection = connection#is_up
let set_up connection = connection#set_up
let set_down connection = connection#set_down

let of_transport ?guid ?up transport =
  match guid with
    | None -> new connection ?guid ?initially_up:up transport
    | Some guid' ->
        match Guid_map.lookup guid' !guid_connection_map with
          | Some connection -> connection
          | None ->
              let connection = new connection ?guid ?initially_up:up transport in
              guid_connection_map := Guid_map.add guid' connection !guid_connection_map;
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

let on_disconnect connection = connection#on_disconnect
let transport connection = connection#transport
let name connection = connection#name
let running connection = connection#running
let watch connection = connection#watch
let add_outgoing_filter connection filter = connection#add_outgoing_filter filter
let add_incoming_filter connection filter = connection#add_incoming_filter filter
let remove_filter = MSet.remove
let close connection = connection#close
