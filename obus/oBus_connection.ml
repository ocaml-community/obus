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
open OBus_intern
open Wire_message
open OBus_types
open Lwt

type guid = OBus_address.guid
type filter_id = int
type filter = OBus_header.any -> OBus_value.sequence -> unit
type name = string

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
  | Wire.Out_of_bounds
  | Wire.Reading_error _
  | OBus_transport.Error _ -> true
  | _ -> false

(* Change the state of the connection to [Crashed] and abort the
   transport *)
let set_crash connection on_crashed ret exn = match !connection with
  | Crashed exn -> on_crashed exn
  | Running running ->
      connection := Crashed exn;
      (* Abort the transport so the dispatcher will exit *)
      begin match OBus_transport.backend running.transport with
        | OBus_transport.Socket fd -> Lwt_unix.abort fd exn
        | _ -> ()
      end;
      remove_connection_of_guid_map running;
      ret

let check_crash connection = match !connection with
  | Crashed exn -> raise exn
  | _ -> ()

let close connection =
  set_crash connection raise () Connection_closed

(* Read the error message of an error *)
let get_error signature context body_ptr = match signature with
  | Tbasic Tstring :: _ ->
      snd $ OBus_wire.rstring context body_ptr
  | _ -> ""

(***** Sending messages *****)

let send_message_backend connection header with_serial signature writer =
  lwt_with_running connection $ fun running ->
    let outgoing = running.outgoing in
    let w = wait () in
    running.outgoing <- w;
    outgoing >>= fun (serial, buffer) ->

      (* Create a new serial *)
      let serial = Int32.succ serial in

      (* Maybe register a reply handler *)
      with_serial running serial;

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
              let exn = set_crash connection (fun x -> x) exn exn in
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
       ((fun header ctx sign ptr body ->
           (* If signature match, just read the content of the
              message with the given reader and wakeup the thread *)
           if sign = osig
           then wakeup w (header, snd (reader ctx ptr))
           else
             (* If not, notify the reply waiter *)
             let `Method_call(path, interf, member) = OBus_header.typ call_header in
             wakeup_exn w $
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
       ((fun header ctx sign ptr body -> wakeup w (header, Lazy.force body)),
        (fun exn -> wakeup_exn w exn))
       running.reply_handlers)

let wire_send_message connection header isig writer =
  send_message_backend connection header (fun _ _ -> ())
    (OBus_types.sequence_of_annot isig) writer

let ksend_message cont connection header typ =
  OBus_comb.func_make_writer typ $ fun writer -> cont
    (wire_send_message connection header (OBus_comb.func_signature typ) writer)

let send_message connection = ksend_message (fun x -> x) connection

let usend_message connection header body =
  send_message_backend connection header (fun _ _ -> ())
    (OBus_value.type_of_sequence body) (OBus_wire.wsequence body)

let wire_send_message_with_reply connection header isig writer osig reader =
  let w = wait () in
  send_message_backend connection header
    (register_reply_handler header w (OBus_types.sequence_of_annot osig) reader)
    (OBus_types.sequence_of_annot isig) writer
  >> w

let ksend_message_with_reply cont connection header typ =
  OBus_comb.func_make_writer typ $ fun writer ->
    let reply = OBus_comb.func_reply typ in
    cont
      (wire_send_message_with_reply connection header
         (OBus_comb.func_signature typ) writer
         (OBus_comb.annot reply) (OBus_comb.reader reply))

let send_message_with_reply connection = ksend_message_with_reply (fun x -> x) connection

let usend_message_with_reply connection header body =
  let w = wait () in
  send_message_backend connection header
    (register_ureply_handler w)
    (OBus_value.type_of_sequence body) (OBus_wire.wsequence body)
  >> w

let ob_string = OBus_comb.make dstring OBus_wire.rstring OBus_wire.wstring
let ob_unit = OBus_comb.make dnil (OBus_wire.return ()) (fun _ -> OBus_wire.return ())

let send_error connection { destination = destination; serial = serial } name msg =
  send_message connection { destination = destination;
                            sender = None;
                            flags = { no_reply_expected = true; no_auto_start = true };
                            serial = 0l;
                            typ = `Error(serial, name) }
    (OBus_comb.abstract ob_string (OBus_comb.reply ob_unit)) msg

let send_exn connection method_call exn =
  match OBus_error.unmake exn with
    | Some(name, msg) ->
        send_error connection method_call name msg
    | None ->
        raise (Invalid_argument
                 (sprintf "not a DBus error: %s" (Printexc.to_string exn)))

(***** Filters *****)

let add_filter connection filter =
  with_running connection $
    fun running ->
      let id = running.next_filter_id in
      running.next_filter_id <- id + 1;
      running.filters <- (id, filter) :: running.filters;
      id

let remove_filter connection id =
  with_running connection $
    fun running -> running.filters <- List.remove_assoc id running.filters

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
  (* Dynamic version of the message. It will probably not be used if
     the programmer only use the high-level interfaces, so we compute
     it only if needed *)
  let dyn_body = lazy (snd $ OBus_wire.rsequence signature context body_ptr) in

  (* First of all, pass the message through all filters *)
  begin match running.filters with
    | [] -> ()
    | l ->
        (* Do only one [Lazy.force] *)
        let body = Lazy.force dyn_body in
        List.iter (fun (id, filter) ->
                     filter header body;
                     (* The connection may have crash during the
                        execution of the filter *)
                     check_crash connection) l
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
               handler header context signature body_ptr dyn_body
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
          (fun  (handler, error_handler) -> error_handler $ OBus_error.make error_name msg)
          (fun _ ->
             DEBUG("error reply to message with serial %ld dropped because no reply was expected, \
                          the error is: %S: %S" reply_serial error_name msg))

    (* For signals we get all the handlers defined for this
       signal. *)
    | { typ = `Signal(path, interface, member) } as header ->
        begin match Signal_map.lookup (interface, member) running.signal_handlers with
          | None ->
              DEBUG("signal %S with signature %S on interface %S, from object %S dropped \
                         because no signal handler where found for this interface and member"
                      member (string_of_signature signature) interface path)
          | Some handlers ->
              List.iter
                (fun (key, reader, handlers) ->
                   (* The explanation of what f is doing is in the
                      comment of [OBus_comb.func.recv] *)
                   let f = snd $ reader context body_ptr in
                   List.iter (fun handler ->
                                f (handler header);
                                check_crash connection) handlers) handlers
        end

    (* Method calls with interface fields, the easy case, we just
       ensure that the sender always get a reply. *)
    | { typ = `Method_call(path, Some(interface), member) } as header ->
        begin match Interf_map.lookup interface running.service_handlers with
          | None ->
              ignore_send_exn connection header $ OBus_error.Unknown_method (sprintf "No such interface: %S" interface)
          | Some handler -> match handler header signature with
              | Mchr_no_such_method ->
                  ignore_send_exn connection header $ OBus_error.Unknown_method
                    (sprintf "Method %S with signature %S on interface %S doesn't exist"
                       member (string_of_signature signature) interface)
              | Mchr_no_such_object ->
                  ignore_send_exn connection header $ OBus_error.Failed (sprintf "No such object: %S" path)
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
                ignore_send_exn connection header $ OBus_error.Unknown_method
                  (sprintf
                     "No interface have a method %S with signature %S on object %S"
                     member (string_of_signature signature) path)
            | [(f, interface)] -> f context body_ptr
            | l ->
                ignore_send_exn connection header $ OBus_error.Failed
                  (sprintf
                     "Ambiguous choice for method %S with signature %S on object %S. \
                          The following interfaces have this method: \"%s\""
                     member (string_of_signature signature) path
                     (String.concat "\", \"" (List.map snd l)))
        end

let traduce = function
  | Wire.Out_of_bounds -> Protocol_error "invalid message size"
  | Wire.Reading_error msg -> Protocol_error msg;
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
            return $ !on_disconnect exn
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
    let connection = ref $ Running {
      transport = transport;
      outgoing = Lwt.return (0l, String.create 65536);
      reply_handlers = Serial_map.empty;
      signal_handlers = Signal_map.empty;
      service_handlers = Interf_map.empty;
      filters = [];
      next_filter_id = 0;
      guid = guid;
      name = None;
      shared = shared;
      on_disconnect = on_disconnect;
    } in
    Lwt.ignore_result $ dispatch_forever connection on_disconnect (String.create 65536);
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
    | Some guid -> return $ of_authenticated_transport ~shared transport guid

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
  with_running connection $ fun running -> running.on_disconnect
let transport connection =
  with_running connection $ fun running -> running.transport
let guid connection =
  with_running connection $ fun running -> running.guid
let name connection =
  with_running connection $ fun running -> running.name
