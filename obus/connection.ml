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
open Header

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

type intern_method_call_handler_result =
  | Intern_mchr_no_such_method
  | Intern_mchr_no_such_object
  | Intern_mchr_ok of (unit -> unit) Wire.body_reader

type body = Values.values

type filter_id = int
type filter = Header.t -> body -> unit

type reply_handler =
  | User of (Header.method_return -> body -> unit)
  | Intern of (Header.method_return -> (unit -> unit) Wire.body_reader)
type signal_handler = signal -> (unit -> unit) Wire.body_reader option
type method_call_handler = method_call -> intern_method_call_handler_result

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

(* Read the body of a message as a [Values.values] value *)
let read_values signature byte_order buffer ptr =
  let ts = Values.dtypes_of_signature signature in
    match byte_order with
      | Wire.Little_endian -> snd (Values.LEReader.read_values ts buffer ptr)
      | Wire.Big_endian -> snd (Values.BEReader.read_values ts buffer ptr)

(* Write the body of a message from a value of type [Values.values] *)
let write_values body byte_order buffer ptr = match byte_order with
  | Wire.Little_endian -> Values.LEWriter.write_values buffer ptr body
  | Wire.Big_endian -> Values.BEWriter.write_values buffer ptr body

(* Read the error message of an error *)
let get_error header byte_order buffer ptr =
  match header.signature with
    | s when s <> "" && s.[0] = 's' ->
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
let write_message connection header body_writer =
  match connection.crashed with
    | Some exn -> raise exn
    | None ->
        try
          Protected.update
            (fun buffer ->
               WireMessage.send_one_message connection.transport buffer header body_writer)
            connection.outgoing_buffer
        with
          | Transport.Error _ as exn ->
              set_crash_and_fail connection exn
          | Wire.Writing_error msg ->
              raise (Cannot_send msg)

(* Sending messages *)

let any_send_message  connection header body_writer =
  let serial = gen_serial connection in
    write_message connection { header with serial = serial } body_writer

let any_send_message_async connection header body_writer on_error handler =
  let serial = gen_serial connection in
    modify_handlers connection
      (fun () -> connection.reply_handlers <-
         SerialMap.add serial (handler, on_error) connection.reply_handlers);
    write_message connection ({ header with serial = serial } : method_call :> Header.t) body_writer

let intern_send_message connection header body_writer =
  any_send_message connection header body_writer
let intern_send_message_async connection header body_writer ?on_error f =
  any_send_message_async connection header body_writer on_error (Intern f)

let make_user_header header body =
  { header with signature =
      Values.signature_of_dtypes
        (Values.dtypes_of_values body) }

let send_message connection header body =
  any_send_message connection (make_user_header header body) (write_values body)
let send_message_async connection header body ?on_error f =
  any_send_message_async connection (make_user_header header body) (write_values body) on_error (User f)

let send_error connection { destination = destination; serial = serial } name msg =
  intern_send_message connection
    (error
       ?destination:destination
       ~reply_serial:serial
       ~error_name:name ())
    (set_error msg)

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

(* Read the body of a method call with the given reader and send an
   error to the sender if the unmarshaling function raise an error. *)
let method_call_read connection method_call reader byte_order buffer ptr =
  try
    [reader byte_order buffer ptr]
  with
      (* These fatal errors are handled by another catcher. The sender
         can of course not be notified. *)
    | Wire.Reading_error _ as exn ->
        raise exn
    | Transport.Error _ as exn ->
        raise exn

    | exn -> match Error.unmake_error exn with
        | Some(name, msg) ->
            send_error connection method_call name msg;
            []
        | None ->
            (* This is really bad, this must not happen *)
            send_error connection method_call
              "org.freedesktop.DBus.Error.Failed"
              (* We can not really be clearer than that. The caml
                 exception will not be usefull to the sender. *)
              "unknown reason";
            raise exn

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
  let header, byte_order, buffer, body_start =
    WireMessage.recv_one_message connection.transport connection.incoming_buffer in
    connection.incoming_buffer <- buffer;
    (* The body is a lazy value so if not needed it is not computed *)
    let body = Lazy.lazy_from_fun (fun () -> read_values header.signature byte_order buffer body_start) in
    let filters, handlers =
      modify_handlers connection
        (fun () ->
           (* First we find all handlers for the message and we
              unmarshal it but we do not call the handlers.

              We do that because handlers may send message and
              wait for reply. So we need to be able to dispatch in
              an handler. For that the incoming buffer must be
              available. *)
           (List.map
              (fun (id, filter) -> let body = Lazy.force body in fun () -> filter header body)
              connection.filters,
            match header with

              (* For method return and errors, we lookup at the reply
                 waiters. If one is find then it get the reply, if
                 none, then the reply is dropped. *)
              | { message_type = `Method_return(reply_serial) } as header ->
                  find_reply_handler connection reply_serial
                    (fun (handler, error_handler) -> match handler with
                       | User f -> let body = Lazy.force body in [fun () -> f header body]
                       | Intern f ->
                           try
                             [f header byte_order buffer body_start]
                           with
                               exn when not (is_fatal exn) ->
                                 (* Always notify the user of
                                    non-fatal errors *)
                                 match error_handler with
                                   | Some f -> [fun () -> f exn]
                                   | None -> raise exn)
                    (fun () ->
                       DEBUG("reply to message with serial %ld dropped" reply_serial))
              | { message_type = `Error(reply_serial, error_name) } ->
                  let msg = get_error header byte_order buffer body_start in
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
              | { message_type = `Signal(path, interface, member) } as header ->
                  begin match InterfMap.lookup interface connection.signal_handlers with
                    | None ->
                        DEBUG("signal %S with signature %S on interface %S, from object %S dropped \
                               because no signal handler where found for this interface"
                                member header.signature interface path);
                        []
                    | Some handlers ->
                        Util.filter_map
                          (fun handler ->
                             match handler header with
                               | Some reader ->
                                   Some(reader byte_order buffer body_start)
                               | None ->
                                   DEBUG("signal %S with signature %S on interface %S, from object %S \
                                          dropped by signal handler"
                                           member header.signature interface path);
                                   None)
                          handlers
                  end

              (* Method calls with interface fields, the easy case, we
                 just ensure that the sender always get a reply. *)
              | { message_type = `Method_call(path, Some(interface), member) } as header ->
                  begin match InterfMap.lookup interface connection.method_call_handlers with
                    | None ->
                        send_error connection header
                          "org.freedesktop.DBus.Error.UnknownMethod"
                          (sprintf "No such interface: %S" interface);
                        []
                    | Some handler -> match handler header with
                        | Intern_mchr_no_such_method ->
                            send_error connection header
                              "org.freedesktop.DBus.Error.UnknownMethod"
                              (sprintf "Method %S with signature %S on interface %S doesn't exist"
                                 member header.signature interface);
                            []
                        | Intern_mchr_no_such_object ->
                            send_error connection header
                              "org.freedesktop.DBus.Error.Failed"
                              (sprintf "No such object: %S" path);
                            []
                        | Intern_mchr_ok reader ->
                            method_call_read connection header reader byte_order buffer body_start
                  end

              (* Method calls without interface fields. We try every
                 interfaces. This implementation choose to send an
                 error if two interfaces have the same method with the
                 same signature for an object *)
              | { message_type = `Method_call(path, None, member) } as header ->
                  begin
                    match
                      InterfMap.fold begin fun interface handler acc ->
                        match handler header with
                          | Intern_mchr_no_such_method
                          | Intern_mchr_no_such_object -> acc
                          | Intern_mchr_ok reader -> (reader, interface) :: acc
                      end connection.method_call_handlers []
                    with
                      | [] ->
                          send_error connection header
                            "org.freedesktop.DBus.Error.UnknownMethod"
                            (sprintf
                               "No interface have a method %S with signature %S on object %S"
                               member header.signature path);
                          []
                      | [(reader, interface)] ->
                          method_call_read connection header reader byte_order buffer body_start
                      | l ->
                          send_error connection header
                            "org.freedesktop.DBus.Error.Failed"
                            (sprintf
                               "Ambiguous choice for method %S with signature %S on object %S. \
                                The following interfaces have this method: \"%s\""
                               member header.signature path
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

let intern_handle_reply_cookie body_reader cookie sleep_mutex =
  Intern(fun header byte_order buffer ptr ->
           cookie := Val(body_reader header byte_order buffer ptr);
           begin match sleep_mutex with
             | Some m -> Mutex.unlock m
             | None -> ()
           end;
           (fun () -> ()))

let user_handle_reply_cookie cookie sleep_mutex =
  User(fun header body ->
         cookie := Val(header, body);
         match sleep_mutex with
           | Some m -> Mutex.unlock m
           | None -> ())

let any_send_message_cookie create_mutex connection header body_writer handler_maker =
  let sleep_mutex = match create_mutex with
    | true
    | false when Thread.self () <> connection.dispatcher ->
        let m = Mutex.create () in
          Mutex.lock m;
          Some m
    | _ -> None
  in
  let cookie = ref (Waiting(connection, sleep_mutex)) in
    any_send_message_async connection header body_writer
      (Some (fun exn -> cookie := Exn exn))
      (handler_maker cookie sleep_mutex);
    cookie

let intern_send_message_cookie connection header body_writer body_reader =
  any_send_message_cookie true connection header body_writer (intern_handle_reply_cookie body_reader)
let send_message_cookie connection header body =
  any_send_message_cookie true connection (make_user_header header body) (write_values body) user_handle_reply_cookie

(* Handling of synchronous call.

   We just create a cookie and retreive it immediatly. *)

let any_send_message_sync connection header body_writer handler_maker =
  let cookie = any_send_message_cookie false connection header body_writer handler_maker in
    intern_cookie_get cookie

let intern_send_message_sync connection header body_writer body_reader =
  any_send_message_sync connection header body_writer (intern_handle_reply_cookie body_reader)
let send_message_sync connection header body =
  any_send_message_sync connection (make_user_header header body) (write_values body) user_handle_reply_cookie
