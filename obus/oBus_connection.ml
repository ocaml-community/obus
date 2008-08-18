(*
 * oBus_connection.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open OBus_header
open Wire
open OBus_internals
open Wire_message
open OBus_type
open OBus_value
open Lwt

type guid = OBus_address.guid
type body = sequence
type filter = OBus_header.any -> body -> unit
type name = string

type filter_id = filter MSet.node
type signal_receiver_id = (signal_match_rule * signal handler) MSet.node

exception Protocol_error of string
exception Connection_closed

type t = connection

(* Mapping from server guid to connection. *)
module Guid_map = My_map(struct type t = guid end)
let guid_connection_map = ref Guid_map.empty

let remove_connection_of_guid_map running =
  if running.shared then guid_connection_map := Guid_map.remove running.guid !guid_connection_map

(***** Error handling *****)

(* This part handle error which can happen on thread other than the
   dispatcher thread.

   There is two reason for which the connection can be crashed by an
   other thread than the dispatcher:

   1. a transport error happen (a message fail to be send)
   2. the connection is explicity closed by the close function

   When this happen the thread which cause the crash set the
   connection state to [Crashed exn], and abort the connection with
   [Lwt_unix.abort].

   The cleanup stuff (which is to notify all waiters that the
   connection has crashed) is always done by the dispatcher thread.
*)

(* Tell weather an error is a fatal error. A fatal error will always
   make the connection to crash. *)
let is_fatal = function
  | Protocol_error _
  | Out_of_bounds
  | Reading_error _
  | OBus_transport.Error _ -> true
  | _ -> false

(* Change the state of the connection to [Crashed] and abort the
   transport.

   Return the exception the connection is really set to and a flag
   telling if the connection was already crashed.
*)
let set_crash connection exn = match !connection with
  | Crashed exn -> (true, exn)
  | Running running ->
      connection := Crashed exn;
      remove_connection_of_guid_map running;
      (* Abort the transport so the dispatcher will exit *)
      begin match OBus_transport.backend running.transport with
        | OBus_transport.Socket fd -> Lwt_unix.abort fd exn
        | _ -> ()
      end;
      (false, exn)

let check_crash connection = match !connection with
  | Crashed exn -> raise exn
  | _ -> ()

let close connection = match set_crash connection Connection_closed with
  | true, exn -> raise exn
  | _ -> ()

(* Read the error message of an error *)
let get_error signature context body_ptr = match signature with
  | Tbasic Tstring :: _ ->
      snd & rstring context body_ptr
  | _ -> ""

(* Header printing (for debugging) *)

let string_of_header header signature =
  let opt = function
    | Some s -> sprintf "%S" s
    | None -> "-"
  in
  sprintf "(%B, %B), %ldl, %s, %s -> %s, %S"
    header.flags.no_reply_expected header.flags.no_auto_start header.serial
    (match header.typ with
       | `Method_call(path, interface, member) ->
           sprintf "`Method_call(%S, %s, %S)" path (opt interface) member
       | `Method_return reply_serial ->
           sprintf "`Method_return(%ldl)" reply_serial
       | `Error(reply_serial, error_name) ->
           sprintf "`Error(%ldl, %S)" reply_serial error_name
       | `Signal(path, interface, member) ->
           sprintf "`Signal(%S, %S, %S)" path interface member)
    (opt header.sender)
    (opt header.destination)
    (string_of_signature signature)

(***** Sending messages *****)

let send_message_backend with_serial signature writer connection header =
  lwt_with_running connection & fun running ->
    let outgoing = running.outgoing in
    let w = wait () in
    running.outgoing <- w;
    outgoing >>= fun (serial, buffer) ->

      (* Create a new serial *)
      let serial = Int32.succ serial in

      (* Maybe register a reply handler *)
      with_serial running serial;

      DEBUG("sending on %S: %s" running.guid (string_of_header header signature));

      (* Write the message *)
      send_one_message connection
        { send_header = header;
          send_serial = serial;
          send_writer = writer;
          send_signature = signature;
          send_byte_order = OBus_info.native_byte_order } buffer

      >>= fun (buffer, exn_opt) ->
        match exn_opt with
          | None ->
              (* Send the new serial/buffer *)
              wakeup w (serial, buffer);
              return ()

          | Some (OBus_transport.Error _ as exn) ->
              (* A fatal error, make the connection to crash *)
              let exn = snd (set_crash connection exn) in
              wakeup_exn w exn;
              fail exn

          | Some exn ->
              (* A non-fatal error, let the connection continue *)
              wakeup w (serial, buffer);
              fail exn

(* Register a reply handler which expect a fixed signature *)
let register_reply_handler call_header w osig reader running serial =
  running.reply_handlers <-
    (Serial_map.add serial
       ((fun header sign ctx ptr body ->
           if sign = osig
           then
             wakeup w (header, snd (reader ctx ptr))
           else
             (* If the signature do not match, notify the reply
                waiter *)
             let `Method_call(path, interf, member) = OBus_header.typ call_header in
             wakeup_exn w &
               Failure (sprintf "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                          member (match interf with Some i -> i | None -> "")
                          (string_of_signature osig)
                          (string_of_signature sign))),
        (fun exn -> wakeup_exn w exn))
       running.reply_handlers)

(* Register an untyped reply handler *)
let register_ureply_handler w running serial =
  running.reply_handlers <-
    (Serial_map.add serial
       ((fun header sign ctx ptr body -> wakeup w (header, Lazy.force body)),
        (fun exn -> wakeup_exn w exn))
       running.reply_handlers)

let ksend_message cont typ =
  ty_function_send typ & fun writer -> cont
    (send_message_backend (fun _ _ -> ()) (isignature typ) writer)

let send_message connection header = ksend_message (fun f -> f connection header)

let dsend_message connection header body =
  send_message_backend (fun _ _ -> ())
    (type_of_sequence body)
    (wsequence body) connection header

let ksend_message_with_reply cont typ =
  ty_function_send typ & fun writer ->
    cont
      (fun connection header ->
         let w = wait () in
         send_message_backend
           (register_reply_handler header w
              (osignature typ)
              (ty_function_reply_reader typ))
           (isignature typ) writer connection header
         >> w)

let send_message_with_reply connection header = ksend_message_with_reply (fun f -> f connection header)

let dsend_message_with_reply connection header body =
  let w = wait () in
  send_message_backend
    (register_ureply_handler w)
    (type_of_sequence body)
    (wsequence body)
    connection header
  >> w

(***** Helpers *****)

let kmethod_call cont ?flags ?sender ?destination ~path ?interface ~member =
  ksend_message_with_reply
    (fun f ->
       cont
         (fun connection ->
            f connection (method_call ?flags ?sender ?destination ~path ?interface ~member ())
            >>= (snd |> return)))

let dmethod_call connection ?flags ?sender ?destination ~path ?interface ~member body =
  dsend_message_with_reply connection (method_call ?flags ?sender ?destination ~path ?interface ~member ()) body
  >>= (snd |> return)

let method_call connection ?flags ?sender ?destination ~path ?interface ~member =
  ksend_message_with_reply
    (fun f ->
       f connection (method_call ?flags ?sender ?destination ~path ?interface ~member ())
       >>= (snd |> return))

let emit_signal connection ?flags ?sender ?destination ~path ~interface ~member =
  send_message connection (signal ?flags ?sender ?destination ~path ~interface ~member ())

let demit_signal connection ?flags ?sender ?destination ~path ~interface ~member =
  dsend_message connection (signal ?flags ?sender ?destination ~path ~interface ~member ())

let send_error connection { destination = destination; serial = serial } name msg =
  send_message connection { destination = destination;
                            sender = None;
                            flags = { no_reply_expected = true; no_auto_start = true };
                            serial = 0l;
                            typ = `Error(serial, name) }
    << string -> unit >> msg

let send_exn connection method_call exn =
  match OBus_error.unmake exn with
    | Some(name, msg) ->
        send_error connection method_call name msg
    | None ->
        raise (Invalid_argument
                 (sprintf "not a DBus error: %s" (Printexc.to_string exn)))

(***** Signals and filters *****)

let add_signal_receiver connection ?sender ?path ?interface ?member typ func =
  with_running connection & fun running ->
    MSet.add running.signal_handlers
      ({ smr_sender = sender;
         smr_path = path;
         smr_interface = interface;
         smr_member = member;
         smr_signature = Some(isignature typ) },
       fun header signature ctx ptr body ->
         ignore (snd (ty_function_recv typ ctx ptr) (func header)))

let dadd_signal_receiver connection ?sender ?path ?interface ?member func =
  with_running connection & fun running ->
    MSet.add running.signal_handlers
      ({ smr_sender = sender;
         smr_path = path;
         smr_interface = interface;
         smr_member = member;
         smr_signature = None },
       fun header signature ctx ptr body -> func header (Lazy.force body))

let add_filter connection filter =
  with_running connection & fun running -> MSet.add running.filters filter

let signal_receiver_enabled = MSet.enabled
let enable_signal_receiver = MSet.enable
let disable_signal_receiver = MSet.disable

let filter_enabled = MSet.enabled
let enable_filter = MSet.enable
let disable_filter = MSet.disable

(***** Reading/dispatching *****)

(* Find the handler for a reply and remove it. *)
let find_reply_handler running serial f g =
  match Serial_map.lookup serial running.reply_handlers with
    | Some x ->
        running.reply_handlers <- Serial_map.remove serial running.reply_handlers;
        f x
    | None ->
        g ()

let ignore_send_exn connection method_call exn = ignore_result (send_exn connection method_call exn)

let dispatch_message connection running header signature context body_ptr =
  DEBUG("received on %S: %s" running.guid (string_of_header header signature));

  (* Dynamic version of the message. It will probably not be used if
     the programmer only use the high-level interfaces, so we compute
     it only if needed *)
  let dyn_body = lazy (snd & rsequence signature context body_ptr) in

  (* First of all, pass the message through all filters *)
  if not (MSet.is_empty running.filters) then begin
    (* Do only one [Lazy.force] *)
    let body = Lazy.force dyn_body in
    MSet.iter (fun filter ->
                 filter header body;
                 (* The connection may have crash during the execution
                    of the filter *)
                 check_crash connection) running.filters
  end;

  (* Now we do the specific dispatching *)
  match header with

    (* For method return and errors, we lookup at the reply
       waiters. If one is find then it get the reply, if none, then
       the reply is dropped. *)
    | { typ = `Method_return(reply_serial) } as header ->
        find_reply_handler running reply_serial
          (fun (handler, error_handler) ->
             try
               handler header signature context body_ptr dyn_body
             with
                 exn when not (is_fatal exn) ->
                   (* Always notify the user of non-fatal
                      errors *)
                   error_handler exn)
          (fun _ ->
             DEBUG("reply to message with serial %ld dropped" reply_serial))

    | { typ = `Error(reply_serial, error_name) } ->
        let msg = get_error signature context body_ptr in
        find_reply_handler running reply_serial
          (fun  (handler, error_handler) -> error_handler & OBus_error.make error_name msg)
          (fun _ ->
             DEBUG("error reply to message with serial %ld dropped because no reply was expected, \
                          the error is: %S: %S" reply_serial error_name msg))

    | { typ = `Signal _ } as header ->
        MSet.iter
          (fun (match_rule, handler) ->
             if signal_match match_rule header signature
             then begin
               handler header signature context body_ptr dyn_body;
               check_crash connection
             end)
          running.signal_handlers

    (* Method calls with interface fields, the easy case, we just
       ensure that the sender always get a reply. *)
    | { typ = `Method_call(path, Some(interface), member) } as header ->
        begin match Interf_map.lookup interface running.service_handlers with
          | None ->
              ignore_send_exn connection header & OBus_error.Unknown_method (sprintf "No such interface: %S" interface)
          | Some handler -> match handler header signature with
              | Mchr_no_such_method ->
                  ignore_send_exn connection header & OBus_error.Unknown_method
                    (sprintf "Method %S with signature %S on interface %S doesn't exist"
                       member (string_of_signature signature) interface)
              | Mchr_no_such_object ->
                  ignore_send_exn connection header & OBus_error.Failed (sprintf "No such object: %S" path)
              | Mchr_ok f -> f context body_ptr
        end

    (* Method calls without interface fields. We try every
       interfaces. This implementation choose to send an error if
       two interfaces have the same method with the same signature
       for an object *)
    | { typ = `Method_call(path, None, member) } as header ->
        begin
          match
            Interf_map.fold begin fun interface handler acc ->
              match handler header signature with
                | Mchr_no_such_method
                | Mchr_no_such_object -> acc
                | Mchr_ok f -> (f, interface) :: acc
            end running.service_handlers []
          with
            | [] ->
                ignore_send_exn connection header & OBus_error.Unknown_method
                  (sprintf
                     "No interface have a method %S with signature %S on object %S"
                     member (string_of_signature signature) path)
            | [(f, interface)] -> f context body_ptr
            | l ->
                ignore_send_exn connection header & OBus_error.Failed
                  (sprintf
                     "Ambiguous choice for method %S with signature %S on object %S. \
                          The following interfaces have this method: \"%s\""
                     member (string_of_signature signature) path
                     (String.concat "\", \"" (List.map snd l)))
        end

let traduce = function
  | Out_of_bounds -> Protocol_error "invalid message size"
  | Reading_error msg -> Protocol_error msg;
  | exn -> exn

let default_on_disconnect exn =
  begin match traduce exn with
    | Protocol_error msg ->
        ERROR("the DBus connection has been closed due to a protocol error: %s" msg)
    | OBus_transport.Error(msg, exn_opt) ->
        ERROR("the DBus connection has been closed due to a transport error: %s%s" msg
                (match exn_opt with
                   | Some exn -> ": " ^ Printexc.to_string exn
                   | None -> ""))
    | exn ->
        ERROR("the DBus connection has been closed due to this uncaught exception: %s" (Printexc.to_string exn))
  end;
  exit 1

let rec dispatch_forever connection on_disconnect buffer = match !connection with
  | Running running ->
      Lwt.bind (recv_one_message connection buffer)
        (fun recv ->
           begin
             try
               dispatch_message connection running
                 recv.recv_header
                 recv.recv_signature
                 (* Unmarshaling context *)
                 { connection = connection;
                   bus_name = recv.recv_header.sender;
                   byte_order = recv.recv_byte_order;
                   buffer = recv.recv_buffer } recv.recv_body_start
             with
                 exn -> match !connection with
                   | Crashed _ -> ()
                   | Running running ->
                       remove_connection_of_guid_map running;
                       connection := Crashed exn
           end;
           dispatch_forever connection running.on_disconnect recv.recv_buffer)
  | Crashed exn -> match exn with
      | Connection_closed -> Lwt.return ()
      | exn ->
          begin try
            return & !on_disconnect exn
          with
              handler_exn ->
                DEBUG("the error handler failed with this exception: %s" (Printexc.to_string handler_exn));
                default_on_disconnect exn
          end
            (*      | exn ->
                    ERROR("uncaught exception on the OBus dispatcher thread: %s" (Printexc.to_string exn));
                    dispatch_forever connection on_disconnect buffer*)

let of_authenticated_transport ?(shared=true) transport guid =
  let make () =
    let on_disconnect = ref default_on_disconnect in
    let connection = ref & Running {
      transport = transport;
      outgoing = Lwt.return (0l, String.create 65536);
      reply_handlers = Serial_map.empty;
      signal_handlers = MSet.make ();
      service_handlers = Interf_map.empty;
      filters = MSet.make ();
      guid = guid;
      name = None;
      shared = shared;
      on_disconnect = on_disconnect;
    } in
    Lwt.ignore_result & dispatch_forever connection on_disconnect (String.create 65536);
    connection
  in
  match shared with
    | false -> make ()
    | true ->
        match Guid_map.lookup guid !guid_connection_map with
          | Some connection -> connection
          | None ->
              let connection = make () in
              guid_connection_map := Guid_map.add guid connection !guid_connection_map;
              connection

let of_transport ?(shared=true) transport =
  OBus_auth.launch transport >>= function
    | None -> fail (Failure "cannot authentificate on the given transport")
    | Some guid -> return & of_authenticated_transport ~shared transport guid

let of_addresses ?(shared=true) addresses = match shared with
  | false -> OBus_transport.of_addresses addresses >>= of_transport ~shared:false
  | true ->
      (* Try to find a guid that we already have *)
      let guids = Util.filter_map (fun ( _, g) -> g) addresses in
      match Util.find_map (fun guid -> Guid_map.lookup guid !guid_connection_map) guids with
        | Some connection -> return connection
        | None ->
            (* We ask again a shared connection even if we know that
               there is no other connection to a server with the
               same guid, because during the authentification
               another thread can add a new connection. *)
            OBus_transport.of_addresses addresses >>= of_transport ~shared:true

let on_disconnect connection =
  with_running connection & fun running -> running.on_disconnect
let transport connection =
  with_running connection & fun running -> running.transport
let guid connection =
  with_running connection & fun running -> running.guid
let name connection =
  with_running connection & fun running -> running.name
