(*
 * connection.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open ThreadImplem
open Message
open WireMessage

module MyMap(Ord : Map.OrderedType) =
struct
  include Map.Make(Ord)

  let lookup key map =
    try
      Some(find key map)
    with
        Not_found -> None
end

module SerialMap = MyMap(struct type t = serial let compare = compare end)
module InterfMap = MyMap(struct type t = string let compare = compare end)

type guid = Address.guid
type name = string

exception Protocol_error of string
exception Cannot_send of string

type intern_handler = unit -> unit
type intern_method_call_handler_result =
  | Intern_mchr_no_such_method
  | Intern_mchr_no_such_object
  | Intern_mchr_ok of intern_handler

type body = Values.values

type filter_id = int
type filter = Message.t -> unit

type reply_handler =
  | User of (method_return -> unit)
  | Intern of (method_return_recv -> intern_handler)
type signal_handler = signal_recv -> intern_handler option
type method_call_handler = method_call_recv -> intern_method_call_handler_result

type connection = {
  (* Transport used for the connection *)
  transport : Transport.t;

  (* Unique name of the connection *)
  name : name option Protected.t;

  (* The server guid *)
  guid : guid;

  (* The dispatcher thread *)
  dispatcher : Thread.t;

  (* This tell weather the connection has crashed *)
  mutable crashed : exn option;

  (* The incomming buffer. Actually only the dispatcher thread access
     to the incoming buffer. Each incoming messages go to this buffer
     and they are immediatly unmarshaled by one of the handlers. It is
     mutable because it can be grown if needed. *)
  mutable incoming_buffer : string;

  (* Outgoing buffer, it can grow too *)
  outgoing_buffer : string Protected.t;

  (* Next available serial for sending message. *)
  next_serial : serial Protected.t;

  (* The handlers mutex. It has two goal: prevent concurrent access to
     structure containing a list of handlers and also prevent addition
     of new handler after the connection crashed. *)
  handlers_mutex : Mutex.t;

  mutable next_filter_id : int;
  mutable filters : (filter_id * filter) list;
  mutable reply_handlers : (reply_handler * (exn -> unit) option) SerialMap.t;
  mutable signal_handlers : signal_handler list InterfMap.t;
  mutable method_call_handlers : method_call_handler InterfMap.t;

  (* Handling of errors *)
  mutable on_error : exn -> bool;
}

type t = connection

(* Mapping from server guid to connection. *)
module GuidTable = Weak.Make
  (struct
     type t = connection
     let equal x y = x.guid = y.guid
     let hash x = Hashtbl.hash x.guid
   end)
let guid_connection_table = GuidTable.create 4
let guid_connection_table_m = Mutex.create ()

let find_guid guid =
  GuidTable.fold begin fun connection acc -> match acc with
    | None -> if connection.guid = guid then Some(connection) else None
    | x -> x
  end guid_connection_table None

let traduce = function
  | Wire.Out_of_bounds -> Protocol_error "invalid message size"
  | Wire.Reading_error msg -> Protocol_error msg;
  | Wire.Writing_error msg -> Cannot_send msg
  | exn -> exn

let is_fatal = function
  | Protocol_error _
  | Wire.Out_of_bounds
  | Wire.Reading_error _
  | Wire.Writing_error _
  | Transport.Error _ -> true
  | _ -> false

let set_crash connection exn =
  connection.crashed <- Some exn;
  (try Transport.close connection.transport () with _ -> ());
  connection.filters <- [];
  connection.reply_handlers <- SerialMap.empty;
  connection.signal_handlers <- InterfMap.empty;
  connection.method_call_handlers <- InterfMap.empty

let set_or_get_crash connection exn =
  with_lock connection.handlers_mutex
    (fun () -> match connection.crashed with
       | Some exn -> exn
       | None -> set_crash connection exn; exn)

let set_crash_and_fail connection exn =
  raise (set_or_get_crash connection exn)

let close connection =
  with_lock connection.handlers_mutex
    (fun () -> match connection.crashed with
       | Some exn -> raise exn
       | None -> set_crash connection (Invalid_argument "connection closed"))

(* Used to safely add or remove an handler. *)
let modify_handlers connection f =
  with_lock connection.handlers_mutex
    (fun () ->
       match connection.crashed with
         | Some exn -> raise exn
         | None -> f ())

(* This generate a new fresh serial *)
let gen_serial connection =
  Protected.process (fun x -> (x, Int32.succ x)) connection.next_serial

(* Read the error message of an error *)
let get_error = function
  | { body = ("s", (byte_order, buffer, ptr)) }  ->
      snd ((match byte_order with
              | Wire.Little_endian -> Wire.LEReader.read_string_string
              | Wire.Big_endian -> Wire.BEReader.read_string_string) buffer ptr)
  | _ -> ""

(* Write an error message *)
let set_error msg byte_order buffer ptr =
  begin match byte_order with
    | Wire.Little_endian -> Wire.LEWriter.write_string_string
    | Wire.Big_endian -> Wire.BEWriter.write_string_string
  end buffer ptr msg

(* Write a whole message on a buffer and send it *)
let write_message connection message =
  match connection.crashed with
    | Some exn -> raise exn
    | None ->
        try
          Protected.update
            (fun buffer ->
               send_one_message connection.transport buffer message)
            connection.outgoing_buffer
        with
          | Transport.Error _ as exn ->
              set_crash_and_fail connection exn
          | Wire.Writing_error msg ->
              raise (Cannot_send msg)

(* Convertion between raw messages and use messages *)

let intern_user_to_send message =
  { message with body =
      (Values.signature_of_dtypes (Values.dtypes_of_values (body message)),
       fun byte_order buffer ptr -> match byte_order with
         | Wire.Little_endian -> Values.LEWriter.write_values buffer ptr (body message)
         | Wire.Big_endian -> Values.BEWriter.write_values buffer ptr (body message)) }

let intern_recv_to_user message =
  let signature, (byte_order, buffer, ptr) = body message in
  let dtypes = Values.dtypes_of_signature signature in
    { message with body = match byte_order with
        | Wire.Little_endian -> snd (Values.LEReader.read_values dtypes buffer ptr)
        | Wire.Big_endian -> snd (Values.BEReader.read_values dtypes buffer ptr) }

(* Sending messages *)

let any_send_message connection message =
  let serial = gen_serial connection in
    write_message connection { message with serial = serial }

let any_send_message_async connection message on_error handler =
  let serial = gen_serial connection in
    modify_handlers connection
      (fun () -> connection.reply_handlers <-
         SerialMap.add serial (handler, on_error) connection.reply_handlers);
    write_message connection ({ message with serial = serial } : method_call_send :> send)

let intern_send_message connection message =
  any_send_message connection message
let intern_send_message_async connection message ?on_error f =
  any_send_message_async connection message on_error (Intern f)

let send_message connection message =
  any_send_message connection (intern_user_to_send message)
let send_message_async connection message ?on_error f =
  any_send_message_async connection (intern_user_to_send message) on_error (User f)

let send_error connection { destination = destination; serial = serial } name msg =
  intern_send_message connection
    { destination = destination;
      sender = None;
      flags = { no_reply_expected = true; no_auto_start = true };
      serial = 0l;
      typ = `Error(serial, name);
      body = ("s", set_error msg) }

let send_exn connection method_call exn =
  match Error.unmake_error exn with
    | Some(name, msg) ->
        send_error connection method_call name msg
    | None ->
        raise (Invalid_argument
                 (sprintf "not a DBus error: %s" (Printexc.to_string exn)))

(* Method call handlers *)

let intern_add_method_call_handler connection interface handler =
  modify_handlers connection
    (fun () -> connection.method_call_handlers <-
       if InterfMap.mem interface connection.method_call_handlers
       then raise (Invalid_argument
                     (sprintf
                        "trying to define multiple method call handlers for interface '%s'"
                        interface))
       else InterfMap.add interface handler connection.method_call_handlers)

(* Signals handlers *)

let intern_add_signal_handler connection interface handler =
  modify_handlers connection
    (fun () -> connection.signal_handlers <-
       InterfMap.add interface
       (handler
        :: (match InterfMap.lookup interface connection.signal_handlers with
              | Some handlers -> handlers
              | None -> []))
       connection.signal_handlers)

(* General filters *)

let add_filter connection filter =
  modify_handlers connection
    (fun () ->
       let id = connection.next_filter_id in
         connection.next_filter_id <- id + 1;
         connection.filters <- (id, filter) :: connection.filters;
         id)
let remove_filter connection id =
  modify_handlers connection
    (fun () ->
       connection.filters <- List.remove_assoc id connection.filters)

(* Find the handler for a reply and remove it. Assume that the
   handlers_mutex is acquired. *)
let find_reply_handler connection serial f g =
  match SerialMap.lookup serial connection.reply_handlers with
    | Some x ->
        connection.reply_handlers <- SerialMap.remove serial connection.reply_handlers;
        f x
    | None ->
        g (); []

(* Handle the case when the handler of method call fail: send an error
   to the sender. *)
let handle_method_call_failure connection method_call exn =
  match Error.unmake_error exn with
    | Some(name, msg) ->
        send_error connection method_call name msg
    | None ->
        (* This is really bad, this must not happen *)
        send_error connection method_call
          "org.freedesktop.DBus.Error.Failed"
          (* We can not really be clearer than that. The caml
             exception will not be usefull to the sender. *)
          "unknown reason";
        raise exn

type unmarshal_result =
  | UR_success of intern_handler
  | UR_failure of exn

let internal_dispatch connection =
  (* This is just to avoid blocking if the connection has crashed but
     this is not guaranteed that it will work. *)
  begin match connection.crashed with
    | Some exn -> raise exn
    | None -> ()
  end;
  (* At this state if the connection crashes, we can block
     forever. But this wil normally never happen since when the
     connection crash the transport is closed and we will get an error
     for that. *)
  let message, buffer = recv_one_message connection.transport connection.incoming_buffer in
    connection.incoming_buffer <- buffer;
    (* User version of the message. It will probably not be used if
       the programmer only use the high-level interfaces, so we
       compute it only if needed *)
    let user_message = Lazy.lazy_from_fun (fun () -> intern_recv_to_user message) in
    let filters, handlers =
      modify_handlers connection
        (fun () ->
           (* First we find all handlers for the message. Each
              handlers, when applied on the marshaled message, will
              return a closure containing its own representation of
              the unmarshaled message. Then after we have find all
              possible handlers we will call them effectively by
              applying the closure to ().

              We do that because handlers may send message and wait
              for reply. So we need to be able to dispatch in a
              handler. For that the incoming buffer must be
              available. *)
           (List.map
              (fun (id, filter) ->
                 let message = Lazy.force user_message in
                   fun () -> filter message)
              connection.filters,

            match message with

              (* For method return and errors, we lookup at the reply
                 waiters. If one is find then it get the reply, if
                 none, then the reply is dropped. *)
              | { typ = `Method_return(reply_serial) } as message ->
                  find_reply_handler connection reply_serial
                    (fun (handler, error_handler) -> match handler with
                       | User f -> let message = intern_recv_to_user message in [fun () -> f message]
                       | Intern f ->
                           try
                             [f message]
                           with
                               exn when not (is_fatal exn) ->
                                 (* Always notify the user of
                                    non-fatal errors *)
                                 match error_handler with
                                   | Some f -> [fun () -> f exn]
                                   | None -> raise exn)
                    (fun () ->
                       DEBUG("reply to message with serial %ld dropped" reply_serial))
              | { typ = `Error(reply_serial, error_name) } ->
                  let msg = get_error message in
                    find_reply_handler connection reply_serial
                      (fun  (handler, error_handler) -> match error_handler with
                         | Some f -> [fun () -> f (Error.make_error error_name msg)]
                         | None ->
                             DEBUG("error reply to message with serial %ld dropped because no on_error is \
                                    provided, the error is: %S: %S"
                                     reply_serial error_name msg);
                             [])
                      (fun () ->
                         DEBUG("error reply to message with serial %ld dropped because no reply was expected, \
                                the error is: %S: %S"
                                 reply_serial error_name msg))

              (* For signals we get all the handlers defined for this
                 signal. Note that with the current implmemtation if
                 multiple handlers are defined for the same interface
                 the message is unmarshaled several times. *)
              | { typ = `Signal(path, interface, member) } as message ->
                  begin match InterfMap.lookup interface connection.signal_handlers with
                    | None ->
                        DEBUG("signal %S with signature %S on interface %S, from object %S dropped \
                               because no signal handler where found for this interface"
                                member (signature message) interface path);
                        []
                    | Some handlers ->
                        Util.filter_map
                          (fun handler ->
                             match handler message with
                               | None ->
                                   DEBUG("signal %S with signature %S on interface %S, from object %S \
                                          dropped by signal handler"
                                           member (signature message) interface path);
                                   None
                               | x -> x)
                          handlers
                  end

              (* Method calls with interface fields, the easy case, we
                 just ensure that the sender always get a reply. *)
              | { typ = `Method_call(path, Some(interface), member) } as message ->
                  begin match InterfMap.lookup interface connection.method_call_handlers with
                    | None ->
                        send_error connection message
                          "org.freedesktop.DBus.Error.UnknownMethod"
                          (sprintf "No such interface: %S" interface);
                        []
                    | Some handler ->
                        match
                          try
                            Some(handler message)
                          with
                              exn when not (is_fatal exn) ->
                                handle_method_call_failure connection message exn;
                                None
                        with
                          | Some Intern_mchr_no_such_method ->
                              send_error connection message
                                "org.freedesktop.DBus.Error.UnknownMethod"
                                (sprintf "Method %S with signature %S on interface %S doesn't exist"
                                   member (signature message) interface);
                              []
                          | Some Intern_mchr_no_such_object ->
                              send_error connection message
                                "org.freedesktop.DBus.Error.Failed"
                                (sprintf "No such object: %S" path);
                              []
                          | Some (Intern_mchr_ok f) -> [f]
                          | None -> []
                  end

              (* Method calls without interface fields. We try every
                 interfaces. This implementation choose to send an
                 error if two interfaces have the same method with the
                 same signature for an object *)
              | { typ = `Method_call(path, None, member) } as message ->
                  begin
                    match
                      InterfMap.fold begin fun interface handler acc ->
                        try
                          match handler message with
                            | Intern_mchr_no_such_method
                            | Intern_mchr_no_such_object -> acc
                            | Intern_mchr_ok f -> (UR_success f, interface) :: acc
                        with
                            exn when not (is_fatal exn) ->
                              (UR_failure exn, interface) :: acc
                      end connection.method_call_handlers []
                    with
                      | [] ->
                          send_error connection message
                            "org.freedesktop.DBus.Error.UnknownMethod"
                            (sprintf
                               "No interface have a method %S with signature %S on object %S"
                               member (signature message) path);
                          []
                      | [(UR_success f, interface)] -> [f]
                      | [(UR_failure exn, interface)] ->
                          handle_method_call_failure connection message exn;
                          []
                      | l ->
                          send_error connection message
                            "org.freedesktop.DBus.Error.Failed"
                            (sprintf
                               "Ambiguous choice for method %S with signature %S on object %S. \
                                The following interfaces have this method: \"%s\""
                               member (signature message) path
                               (String.concat "\", \"" (List.map snd l)));
                          []
                  end))
    in
      (* Now we can run all handlers *)
      List.iter (fun filter -> filter ()) filters;
      List.iter (fun handler -> handler ()) handlers

let really_dispatch connection =
  try
    internal_dispatch connection
  with
      exn when is_fatal exn ->
        set_crash_and_fail connection (traduce exn)

let dispatch connection =
  if Thread.self () = connection.dispatcher
  then really_dispatch connection
  else ()

let default_on_error exn =
  begin match exn with
    | Protocol_error msg ->
        ERROR("the DBus connection has been closed due to a protocol error: %s" msg)
    | Transport.Error(msg, exn_opt) ->
        ERROR("the DBus connection has been closed due to a transport error: %s%s" msg
                (match exn_opt with
                   | Some exn -> " :" ^ Printexc.to_string exn
                   | None -> ""))
    | exn ->
        ERROR("the DBus connection has been closed due to this uncaught exception: %s" (Printexc.to_string exn))
  end;
  exit 1

let catch_error connection exn =
  try
    connection.on_error exn
  with
      handler_exn ->
        DEBUG("the error handler failed with this exception: %s" (Printexc.to_string handler_exn));
        default_on_error exn

let rec dispatch_forever connection =
  match
    try
      internal_dispatch connection;
      true
    with
      | exn when is_fatal exn ->
          ignore (catch_error connection (set_or_get_crash connection (traduce exn)));
          false
      | exn ->
          catch_error connection exn
  with
    | true -> dispatch_forever connection
    | false -> ()

let init_dispatcher receive_connection initial_mutex =
  Mutex.lock initial_mutex;
  match !receive_connection with
    | Some connection -> dispatch_forever connection
    | None -> assert false

let of_authenticated_transport ?(shared=true) transport guid =
  let make () =
    let initial_dispatcher_mutex = Mutex.create ()
    and send_connection = ref None in
      Mutex.lock initial_dispatcher_mutex;
      let connection = {
        transport = transport;
        dispatcher = Thread.create (init_dispatcher send_connection) initial_dispatcher_mutex;
        incoming_buffer = String.create 65536;
        outgoing_buffer = Protected.make (String.create 65536);
        reply_handlers = SerialMap.empty;
        signal_handlers = InterfMap.empty;
        method_call_handlers = InterfMap.empty;
        filters = [];
        handlers_mutex = Mutex.create ();
        crashed = None;
        next_serial = Protected.make 1l;
        next_filter_id = 0;
        guid = guid;
        name = Protected.make None;
        on_error = default_on_error;
      } in
        send_connection := Some connection;
        Mutex.unlock initial_dispatcher_mutex;
        connection
  in
  let connection = match shared with
    | false -> make ()
    | true ->
        with_lock guid_connection_table_m begin fun () ->
          match find_guid guid with
            | Some(connection) ->
                connection
            | None ->
                let connection = make () in
                  GuidTable.add guid_connection_table connection;
                  connection
        end
  in
    connection

let of_transport ?(shared=true) transport =
  match Auth.launch transport with
    | None -> failwith "cannot authentificate on the given transport"
    | Some(guid) -> of_authenticated_transport ~shared:shared transport guid

let of_addresses ?(shared=true) addresses = match shared with
  | false -> of_transport (Transport.of_addresses addresses) ~shared:false
  | true ->
      (* Try to find an guid that we already have *)
      let guids = Util.filter_map (fun (_, _, g) -> g) addresses in
        match
          with_lock guid_connection_table_m
            (fun () -> Util.find_map find_guid guids)
        with
          | Some(connection) -> connection
          | None ->
              (* We ask again a shared connection even if we know that
                 there is no other connection to a server with the
                 same guid, because between the two lookup another
                 thread can add a new connection. *)
              of_transport ~shared:true (Transport.of_addresses addresses)

let transport connection = connection.transport
let guid connection = connection.guid

let on_error connection f =
  modify_handlers connection
    (fun () ->
       connection.on_error <- f)

let intern_get_name connection f =
  Protected.if_none connection.name f

(* Handling of cookie.

   They are just implemented as asynchronous call which store the
   reply into a shared reference. *)

type 'a content =
  | Waiting of connection * Mutex.t option
      (* The reply has not already come *)
  | Val of 'a
      (* The reply come and this is the readed value *)
  | Exn of exn
      (* An exception has been raised during the reading of the
         message *)

type 'a cookie = 'a content ref

let rec intern_cookie_get cookie = match !cookie with
  | Waiting(connection, w) ->
      begin match w with
        | Some m when Thread.self () <> connection.dispatcher ->
            Mutex.lock m;
            Mutex.unlock m
        | _ ->
            really_dispatch connection
      end;
      intern_cookie_get cookie
  | Exn exn -> raise exn
  | Val v -> v

let intern_cookie_get_if_ready cookie = match !cookie with
  | Waiting _ -> None
  | Val v -> Some v
  | Exn exn -> raise exn

let intern_cookie_is_ready cookie = match !cookie with
  | Waiting _ -> false
  | _ -> true

let intern_cookie_is_value cookie = match !cookie with
  | Val _ -> true
  | _ -> false

let intern_cookie_is_exn cookie = match !cookie with
  | Exn _ -> true
  | _ -> false

let intern_handle_reply_cookie handler cookie sleep_mutex =
  Intern(fun message ->
           cookie := Val(handler message);
           begin match sleep_mutex with
             | Some m -> Mutex.unlock m
             | None -> ()
           end;
           (fun () -> ()))

let user_handle_reply_cookie cookie sleep_mutex =
  User(fun message ->
         cookie := Val message;
         match sleep_mutex with
           | Some m -> Mutex.unlock m
           | None -> ())

let any_send_message_cookie create_mutex connection message handler_maker =
  let sleep_mutex = match create_mutex with
    | true
    | false when Thread.self () <> connection.dispatcher ->
        let m = Mutex.create () in
          Mutex.lock m;
          Some m
    | _ -> None
  in
  let cookie = ref (Waiting(connection, sleep_mutex)) in
    any_send_message_async connection message
      (Some (fun exn -> cookie := Exn exn))
      (handler_maker cookie sleep_mutex);
    cookie

let intern_send_message_cookie connection message body_reader =
  any_send_message_cookie true connection message (intern_handle_reply_cookie body_reader)
let send_message_cookie connection message =
  any_send_message_cookie true connection (intern_user_to_send message) user_handle_reply_cookie

(* Handling of synchronous call.

   We just create a cookie and retreive it immediatly. *)

let any_send_message_sync connection message handler_maker =
  let cookie = any_send_message_cookie false connection message handler_maker in
    intern_cookie_get cookie

let intern_send_message_sync connection message body_reader =
  any_send_message_sync connection message (intern_handle_reply_cookie body_reader)
let send_message_sync connection message =
  any_send_message_sync connection (intern_user_to_send message) user_handle_reply_cookie
