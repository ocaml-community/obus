(*
 * oBus_connection.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(connection)"

open Lwt_react
open Lwt.Infix

(* +-----------------------------------------------------------------+
   | Exceptions                                                      |
   +-----------------------------------------------------------------+ *)

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

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

module Serial_map = Map.Make
  (struct
     type t = OBus_message.serial
     let compare : int32 -> int32 -> int = compare
   end)

module Int_map = Map.Make
  (struct
     type t = int
     let compare : int -> int -> int = compare
   end)

type filter = OBus_message.t -> OBus_message.t option
  (* Type of message filters *)

(* Connection are wrapped into object in order to make them
   comparable. In the code, wrapped connection are simply referred has
   "connection" and internal connection details are referred as
   "active". *)

(* Type of active connections *)
type active_connection = {
  mutable name : OBus_name.bus;
  (* The name of the connection in case the endpoint is a message bus,
     or [""] if not. *)

  transport : OBus_transport.t;
  (* The transport used for messages *)

  mutable on_disconnect : exn -> unit Lwt.t;
  (* [on_disconnect] is called the connection is closed
     prematurely. This happen on transport errors. *)

  guid : OBus_address.guid option;
  (* Guid of the connection. It may is [Some guid] if this is the
     client-side part of a peer-to-peer connection and the connection
     is shared. *)

  down : (unit Lwt.t * unit Lwt.u) option signal;
  set_down : (unit Lwt.t * unit Lwt.u) option -> unit;
  (* Waiting thread used to make the connection to stop dispatching
     messages. *)

  state : [ `Up | `Down ] signal;

  abort_recv_wakener : OBus_message.t Lwt.u;
  abort_send_wakener : unit Lwt.u;
  abort_recv_waiter : OBus_message.t Lwt.t;
  abort_send_waiter : unit Lwt.t;
  (* Waiting threads wakeup when the connection is closed or
     aborted. It is used to make the dispatcher/writer to exit. *)

  mutable next_serial : OBus_message.serial;
  (* The first available serial, incremented for each message *)

  mutable outgoing_mutex : Lwt_mutex.t;
  (* Mutex used to serialise message sending *)

  incoming_filters : filter Lwt_sequence.t;
  outgoing_filters : filter Lwt_sequence.t;

  mutable reply_waiters : OBus_message.t Lwt.u Serial_map.t;
  (* Mapping serial -> thread waiting for a reply *)

  mutable data : exn Int_map.t;
  (* Set of locally stored values *)

  wrapper : t;
  (* The wrapper containing the connection *)
}

(* State of a connection *)
and connection_state =
  | Active of active_connection
      (* The connection is currently active *)
  | Closed
      (* The connection has been closed gracefully *)
  | Killed
      (* The connection has been killed after an error happened *)

(* Connections are packed into objects to make them comparable *)
and t = <
  state : connection_state;
  (* Get the connection state *)

  set_state : connection_state -> unit;
  (* Sets the state of the connection *)

  get : active_connection;
  (* Returns the connection if it is active, and fail otherwise *)

  active : bool signal;
  (* Signal holding the current connection state. *)
>

let compare : t -> t -> int = Pervasives.compare

(* +-----------------------------------------------------------------+
   | Guids                                                           |
   +-----------------------------------------------------------------+ *)

(* Mapping from server guid to connection. *)
module Guid_map = Map.Make(struct
                            type t = OBus_address.guid
                            let compare = Pervasives.compare
                          end)

let guid_connection_map = ref Guid_map.empty

(* +-----------------------------------------------------------------+
   | Filters                                                         |
   +-----------------------------------------------------------------+ *)

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
   | Connection closing                                              |
   +-----------------------------------------------------------------+ *)

let cleanup active ~is_crash =
  begin
    match active.guid with
      | Some guid ->
          guid_connection_map := Guid_map.remove guid !guid_connection_map
      | None ->
          ()
  end;

  (* This make the dispatcher to exit if it is waiting on
     [get_message] *)
  Lwt.wakeup_exn active.abort_recv_wakener Connection_closed;
  begin
    match S.value active.down with
      | Some(waiter, wakener) ->
          Lwt.wakeup_exn wakener Connection_closed
      | None ->
          ()
  end;

  (* Wakeup all reply handlers so they will not wait forever *)
  Serial_map.iter (fun _ wakener -> Lwt.wakeup_exn wakener Connection_closed) active.reply_waiters;

  (* If the connection is closed normally, flush it *)
  let%lwt () =
    if not is_crash then
      Lwt_mutex.with_lock active.outgoing_mutex Lwt.return
    else begin
      Lwt.wakeup_exn active.abort_send_wakener Connection_closed;
      Lwt.return ()
    end
  in

  (* Shutdown the transport *)
  try%lwt
    OBus_transport.shutdown active.transport
  with exn ->
    Lwt_log.error ~section ~exn "failed to abort/shutdown the transport"

let close connection =
  match connection#state with
    | Killed | Closed ->
        Lwt.return ()
    | Active active ->
        connection#set_state Closed;
        cleanup active ~is_crash:false

let kill connection exn =
  match connection#state with
    | Killed | Closed ->
        Lwt.return ()
    | Active active ->
        connection#set_state Killed;
        let%lwt () = cleanup active ~is_crash:true in
        try%lwt
          active.on_disconnect exn
        with exn ->
          Lwt_log.error ~section ~exn "the error handler failed with"

(* +-----------------------------------------------------------------+
   | Sending messages                                                 |
   +-----------------------------------------------------------------+ *)

(* Send a message, maybe adding a reply waiter and return
   [return_thread] *)
let send_message_backend connection gen_serial reply_waiter_opt message =
  let active = connection#get in
  Lwt_mutex.with_lock active.outgoing_mutex
    (fun () ->
       let send_it, closed = match connection#state with
         | Active _ ->
             (true, false)
         | Closed ->
             (* Flush the connection if closed gracefully *)
             (true, true)
         | Killed ->
             (false, true)
       in
       if send_it then begin
         let message = if gen_serial then { message with OBus_message.serial = active.next_serial } else message in
         match apply_filters "outgoing" message active.outgoing_filters with
           | None ->
               let%lwt () = Lwt_log.debug ~section "outgoing message dropped by filters" in
               Lwt.fail (Failure "message dropped by filters")

           | Some message ->
               if not closed then begin
                 match reply_waiter_opt with
                   | Some(waiter, wakener) ->
                       active.reply_waiters <- Serial_map.add (OBus_message.serial message) wakener active.reply_waiters;
                       Lwt.on_cancel waiter (fun () ->
                                               match connection#state with
                                                 | Killed | Closed ->
                                                     ()
                                                 | Active active ->
                                                     active.reply_waiters <- Serial_map.remove (OBus_message.serial message) active.reply_waiters)
                   | None ->
                       ()
               end;

               try%lwt
                 let%lwt () = Lwt.choose [active.abort_send_waiter;
                                          (* Do not cancel a thread while it is marshaling message: *)
                                          Lwt.protected (OBus_transport.send active.transport message)] in
                 (* Everything went OK, continue with a new serial *)
                 if gen_serial then active.next_serial <- Int32.succ active.next_serial;
                 Lwt.return ()
               with
                 | OBus_wire.Data_error _ as exn ->
                     (* The message can not be marshaled for some
                        reason. This is not a fatal error. *)
                     Lwt.fail exn

                 | Lwt.Canceled ->
                     (* Message sending have been canceled by the
                        user. This is not a fatal error either. *)
                     Lwt.fail Lwt.Canceled

                 | exn ->
                     (* All other errors are considered as fatal. They
                        are fatal because it is possible that a
                        message has been partially sent on the
                        connection, so the message stream is broken *)
                     let%lwt () = kill connection exn in
                     Lwt.fail exn
       end else
         match connection#state with
           | Killed | Closed ->
               Lwt.fail Connection_closed
           | Active _ ->
               Lwt.return ())

let send_message connection message =
  send_message_backend connection true None message

let send_message_with_reply connection message =
  let (waiter, wakener) as v = Lwt.task () in
  let%lwt () = send_message_backend connection true (Some v) message in
  waiter

let send_message_keep_serial connection message =
  send_message_backend connection false None message

let send_message_keep_serial_with_reply connection message =
  let (waiter, wakener) as v = Lwt.task () in
  let%lwt () = send_message_backend connection false (Some v) message in
  waiter

(* +-----------------------------------------------------------------+
   | Helpers for calling methods                                     |
   +-----------------------------------------------------------------+ *)

let method_call_with_message ~connection ?destination ~path ?interface ~member ~i_args ~o_args args =
  let i_msg =
    OBus_message.method_call
      ?destination
      ~path
      ?interface
      ~member
      (OBus_value.C.make_sequence i_args args)
  in
  let%lwt o_msg = send_message_with_reply connection i_msg in
  match o_msg with
    | { OBus_message.typ = OBus_message.Method_return _; body } -> begin
        try
          Lwt.return (o_msg, OBus_value.C.cast_sequence o_args body)
        with OBus_value.C.Signature_mismatch ->
          Lwt.fail (OBus_message.invalid_reply i_msg (OBus_value.C.type_sequence o_args) o_msg)
      end
    | { OBus_message.typ = OBus_message.Error(_, error_name);
        OBus_message.body = OBus_value.V.Basic(OBus_value.V.String message) :: _  } ->
        Lwt.fail (OBus_error.make error_name message)
    | { OBus_message.typ = OBus_message.Error(_, error_name) } ->
        Lwt.fail (OBus_error.make error_name "")
    | _ ->
        assert false

let method_call ~connection ?destination ~path ?interface ~member ~i_args ~o_args args =
  method_call_with_message ~connection ?destination ~path ?interface ~member ~i_args ~o_args args >|= snd

let method_call_no_reply ~connection ?destination ~path ?interface ~member ~i_args args =
  send_message connection
    (OBus_message.method_call
       ~flags:{ OBus_message.default_flags with OBus_message.no_reply_expected = true }
       ?destination
       ~path
       ?interface
       ~member
       (OBus_value.C.make_sequence i_args args))

(* +-----------------------------------------------------------------+
   | Reading/dispatching                                             |
   +-----------------------------------------------------------------+ *)

let dispatch_message active message =
  let open OBus_message in
  match message with

    (* For method return and errors, we lookup at the reply waiters. If
       one is find then it get the reply, if none, then the reply is
       dropped. *)
    | { typ = Method_return(reply_serial) }
    | { typ = Error(reply_serial, _) } -> begin
        match try Some(Serial_map.find reply_serial active.reply_waiters) with Not_found -> None with
          | Some w ->
              active.reply_waiters <- Serial_map.remove reply_serial active.reply_waiters;
              Lwt.wakeup w message;
              Lwt.return ()
          | None ->
              Lwt_log.debug_f ~section "reply to message with serial %ld dropped%s"
                reply_serial
                (match message with
                   | { typ = Error(_, error_name) } ->
                       Printf.sprintf ", the reply is the error: %S: %S"
                         error_name
                         (match message.body with
                            | OBus_value.V.Basic(OBus_value.V.String x) :: _ -> x
                            | _ -> "")
                   | _ ->
                       "")
      end

    (* Handling of the special "org.freedesktop.DBus.Peer" interface *)
    | { typ = Method_call(_, "org.freedesktop.DBus.Peer", member); body; sender; serial } -> begin
        try%lwt
          let%lwt body =
            match member, body with
              | "Ping", [] ->
                  Lwt.return []
              | "GetMachineId", [] -> begin
                  try%lwt
                    let%lwt uuid = Lazy.force OBus_info.machine_uuid in
                    Lwt.return [OBus_value.V.basic_string (OBus_uuid.to_string uuid)]
                  with exn ->
                    if OBus_error.name exn = OBus_error.ocaml then
                      Lwt.fail
                        (OBus_error.Failed
                           (Printf.sprintf
                              "Cannot read the machine uuid file (%s)"
                              OBus_config.machine_uuid_file))
                    else
                      Lwt.fail exn
                end
              | _ ->
                  Lwt.fail
                    (OBus_error.Unknown_method
                       (Printf.sprintf
                          "Method %S with signature %S on interface \"org.freedesktop.DBus.Peer\" does not exist"
                          member
                          (OBus_value.string_of_signature (OBus_value.V.type_of_sequence body))))
          in
          send_message active.wrapper {
            flags = { no_reply_expected = true; no_auto_start = true };
            serial = 0l;
            typ = Method_return serial;
            destination = sender;
            sender = "";
            body = body;
          }
        with exn ->
          let name, msg = OBus_error.cast exn in
          send_message active.wrapper {
            flags = { no_reply_expected = true; no_auto_start = true };
            serial = 0l;
            typ = Error(serial, name);
            destination = sender;
            sender = "";
            body = [OBus_value.V.basic_string msg];
          }
      end

    | _ ->
        (* Other messages are handled by specifics modules *)
        Lwt.return ()

let rec dispatch_forever active =
  let%lwt () =
    (* Wait for the connection to become up *)
    match S.value active.down with
      | Some(waiter, wakener) ->
          waiter
      | None ->
          Lwt.return ()
  in
  let%lwt message =
    try%lwt
      Lwt.choose [OBus_transport.recv active.transport; active.abort_recv_waiter]
    with exn ->
      let%lwt () = kill active.wrapper (Transport_error exn) in
      Lwt.fail exn
  in
  match apply_filters "incoming" message active.incoming_filters with
    | None ->
        let%lwt () = Lwt_log.debug ~section "incoming message dropped by filters" in
        dispatch_forever active
    | Some message ->
        (* The internal dispatcher accepts only messages destined to
           the current connection: *)
        if active.name = "" || OBus_message.destination message = active.name then ignore (
          (try%lwt
            dispatch_message active message
          with exn ->
            Lwt_log.error ~section ~exn "message dispatching failed with")
          [%lwt.finally
            OBus_value.V.sequence_close (OBus_message.body message)]
        );
        dispatch_forever active

(* +-----------------------------------------------------------------+
   | Connection creation                                             |
   +-----------------------------------------------------------------+ *)

class connection () =
  let active, set_active = S.create false in
object(self)

  method active = active

  val mutable state = Closed

  method state = state

  method set_state new_state =
    state <- new_state;
    match state with
      | Closed | Killed ->
          set_active false
      | Active _ ->
          set_active true

  method get =
    match state with
      | Closed | Killed -> raise Connection_closed
      | Active active -> active
end

let of_transport ?switch ?guid ?(up=true) transport =
  Lwt_switch.check switch;
  let make () =
    let abort_recv_waiter, abort_recv_wakener = Lwt.wait ()
    and abort_send_waiter, abort_send_wakener = Lwt.wait ()
    and connection = new connection ()
    and down, set_down = S.create (if up then None else Some(Lwt.wait ())) in
    let state = S.map (function None -> `Up | Some _ -> `Down) down in
    let active = {
      name = "";
      transport;
      on_disconnect = (fun exn -> Lwt.return ());
      guid;
      down;
      set_down;
      state;
      abort_recv_waiter;
      abort_send_waiter;
      abort_recv_wakener = abort_recv_wakener;
      abort_send_wakener = abort_send_wakener;
      outgoing_mutex = Lwt_mutex.create ();
      next_serial = 1l;
      incoming_filters = Lwt_sequence.create ();
      outgoing_filters = Lwt_sequence.create ();
      reply_waiters = Serial_map.empty;
      data = Int_map.empty;
      wrapper = connection;
    } in
    connection#set_state (Active active);
    (* Start the dispatcher *)
    ignore (dispatch_forever active);
    Lwt_switch.add_hook switch (fun () -> close connection);
    connection
  in
  match guid with
    | None ->
        make ()
    | Some guid ->
        match try Some(Guid_map.find guid !guid_connection_map) with Not_found -> None with
          | Some connection ->
              Lwt_switch.add_hook switch (fun () -> close connection);
              connection
          | None ->
              let connection = make () in
              guid_connection_map := Guid_map.add guid connection !guid_connection_map;
              connection

(* Capabilities turned on by default: *)
let capabilities = [`Unix_fd]

let of_addresses ?switch ?(shared=true) addresses =
  Lwt_switch.check switch;
  match shared with
    | false ->
        let%lwt guid, transport = OBus_transport.of_addresses ~capabilities addresses in
        Lwt.return (of_transport ?switch transport)
    | true ->
        (* Try to find a guid that we already have *)
        let guids = OBus_util.filter_map OBus_address.guid addresses in
        match OBus_util.find_map (fun guid -> try Some(Guid_map.find guid !guid_connection_map) with Not_found -> None) guids with
          | Some connection ->
              Lwt_switch.add_hook switch (fun () -> close connection);
              Lwt.return connection
          | None ->
              (* We ask again a shared connection even if we know that
                 there is no other connection to a server with the same
                 guid, because during the authentication another
                 thread can add a new connection. *)
              let%lwt guid, transport = OBus_transport.of_addresses ~capabilities addresses in
              Lwt.return (of_transport ?switch ~guid transport)

let loopback () = of_transport (OBus_transport.loopback ())

(* +-----------------------------------------------------------------+
   | Local storage                                                   |
   +-----------------------------------------------------------------+ *)

type 'a key = {
  key_id : int;
  key_make : 'a -> exn;
  key_cast : exn -> 'a;
}

let next_key_id = ref 0

let new_key (type t) () =
  let key_id = !next_key_id in
  next_key_id := key_id + 1;
  let module M = struct exception E of t end in
  {
    key_id = key_id;
    key_make = (fun x -> M.E x);
    key_cast = (function M.E x -> x | _ -> assert false);
  }

let get connection key =
  let active = connection#get in
  try
    let cell = Int_map.find key.key_id active.data in
    Some(key.key_cast cell)
  with Not_found ->
    None

let set connection key value =
  let active = connection#get in
  match value with
    | Some x ->
        active.data <- Int_map.add key.key_id (key.key_make x) active.data
    | None ->
        active.data <- Int_map.remove key.key_id active.data

(* +-----------------------------------------------------------------+
   | Other                                                           |
   +-----------------------------------------------------------------+ *)

let name connection = connection#get.name
let set_name connection name = connection#get.name <- name

let active connection = connection#active

let guid connection = connection#get.guid
let transport connection = connection#get.transport

let can_send_basic_type connection = function
  | OBus_value.T.Unix_fd -> List.mem `Unix_fd (OBus_transport.capabilities connection#get.transport)
  | _ -> true

let rec can_send_single_type connection = function
  | OBus_value.T.Basic t -> can_send_basic_type connection t
  | OBus_value.T.Array t -> can_send_single_type connection t
  | OBus_value.T.Dict(tk, tv) -> can_send_basic_type connection tk && can_send_single_type connection tv
  | OBus_value.T.Structure tl -> List.for_all (can_send_single_type connection) tl
  | OBus_value.T.Variant -> true

let can_send_sequence_type connection tl = List.for_all (can_send_single_type connection) tl

let set_on_disconnect connection f =
  match connection#state with
    | Closed | Killed ->
        ()
    | Active active ->
        active.on_disconnect <- f

let state connection = connection#get.state

let set_up connection =
  let active = connection#get in
  match S.value active.down with
    | None ->
        ()
    | Some(waiter, wakener) ->
        active.set_down None;
        Lwt.wakeup wakener ()

let set_down connection =
  let active = connection#get in
  match S.value active.down with
    | Some _ ->
        ()
    | None ->
        active.set_down (Some(Lwt.wait ()))

let incoming_filters connection = connection#get.incoming_filters
let outgoing_filters connection = connection#get.outgoing_filters
