(*
 * oBus_connection.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open OBus_message
open OBus_internals
open Wire
open Wire_message
open OBus_type
open OBus_value
open Lwt

type guid = OBus_address.guid
type name = string

type filter_id = filter MSet.node
type signal_receiver = (signal_match_rule * signal handler) MSet.node

exception Protocol_error of string
exception Invalid_data of string
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

(* Get the error message of an error *)
let get_error msg = match msg.body with
  | Basic String x :: _ -> x
  | _ -> ""

(* Message printing (for debugging) *)

let string_of_message message =
  let opt = function
    | Some s -> sprintf "%S" s
    | None -> "-"
  in
  sprintf "(%B, %B), %ldl, %s, %s -> %s, %s"
    message.flags.no_reply_expected message.flags.no_auto_start message.serial
    (match message.typ with
       | `Method_call(path, interface, member) ->
           sprintf "`Method_call(%S, %s, %S)" path (opt interface) member
       | `Method_return reply_serial ->
           sprintf "`Method_return(%ldl)" reply_serial
       | `Error(reply_serial, error_name) ->
           sprintf "`Error(%ldl, %S)" reply_serial error_name
       | `Signal(path, interface, member) ->
           sprintf "`Signal(%S, %S, %S)" path interface member)
    (opt message.sender)
    (opt message.destination)
    (string_of_sequence message.body)

(***** Sending messages *****)

let send_message_backend with_serial connection message =
  lwt_with_running connection & fun running ->
    let outgoing = running.outgoing in
    let w = wait () in
    running.outgoing <- w;
    outgoing >>= fun serial ->

      (* Create a new serial *)
      let serial = Int32.succ serial in

      (* Maybe register a reply handler *)
      with_serial running serial;

      DEBUG("sending on %S: %s" running.guid (string_of_message message));

      (* Write the message *)
      try_bind (fun _ -> send_one_message connection { message with serial = serial })
        (fun _ ->
           (* Send the new serial/buffer *)
           wakeup w serial;
           return ())
        (function

           (* A fatal error, make the connection to crash *)
           | OBus_transport.Error _ as exn ->
               let exn = snd (set_crash connection exn) in
               wakeup_exn w exn;
               fail exn

           (* A non-fatal error, let the connection continue *)
           | Writing_error msg ->
               wakeup w serial;
               fail & Invalid_data msg
           | exn ->
               wakeup w serial;
               fail exn)

let send_message connection message = send_message_backend (fun _ _ -> ()) connection message

let send_message_with_reply connection message =
  let w = wait () in
  send_message_backend
    (fun running serial ->
       running.reply_handlers <-
         (Serial_map.add serial (wakeup w, wakeup_exn w) running.reply_handlers))
    connection message
  >>= fun _ -> w

(***** Helpers *****)

let call_and_cast_reply ty cont =
  make_func ty & fun body ->
    cont body
      (fun connection message ->
         send_message_with_reply connection message >>= fun msg ->
           match opt_cast_sequence ~context:(connection, msg.sender) (func_reply ty) msg.body with

             (* If the cast success, just return the result *)
             | Some x -> return x

             (* If not, check why the cast fail *)
             | None ->
                 let expected_sig = osignature ty
                 and got_sig = type_of_sequence msg.body in
                 if expected_sig = got_sig
                 then
                   (* If the signature match, this means that the
                      user defined a combinator raising a
                      Cast_failure *)
                   fail Cast_failure
                 else
                   (* In other case this means that the expected
                      signature is wrong *)
                   let { typ = `Method_call(path, interf, member) } = message in
                   fail &
                     Failure (sprintf "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                                member (match interf with Some i -> i | None -> "")
                                (string_of_signature expected_sig)
                                (string_of_signature got_sig)))

let dmethod_call connection ?flags ?sender ?destination ~path ?interface ~member body =
  send_message_with_reply connection
    (method_call ?flags ?sender ?destination ~path ?interface ~member body)
  >>= fun { body = x } -> return x

let kmethod_call cont ?flags ?sender ?destination ~path ?interface ~member ty =
  call_and_cast_reply ty & fun body f ->
    cont (fun connection ->
            f connection (method_call ?flags ?sender ?destination ~path ?interface ~member body))

let method_call connection ?flags ?sender ?destination ~path ?interface ~member ty =
  call_and_cast_reply ty & fun body f ->
    f connection (method_call ?flags ?sender ?destination ~path ?interface ~member body)

let emit_signal connection ?flags ?sender ?destination ~path ~interface ~member ty =
  make_func ty & fun body ->
    send_message connection (signal ?flags ?sender ?destination ~path ~interface ~member body)

let demit_signal connection ?flags ?sender ?destination ~path ~interface ~member body =
  send_message connection (signal ?flags ?sender ?destination ~path ~interface ~member body)

let send_error connection { destination = destination; serial = serial } name msg =
  send_message connection { destination = destination;
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
        raise (Invalid_argument
                 (sprintf "not a DBus error: %s" (Printexc.to_string exn)))

(***** Signals and filters *****)

let add_signal_receiver connection ?sender ?destination ?path ?interface ?member ?(args=[]) typ func =
  with_running connection & fun running ->
    MSet.add running.signal_handlers
      ({ smr_sender = sender;
         smr_destination = destination;
         smr_path = path;
         smr_interface = interface;
         smr_member = member;
         smr_args = args },
       fun msg -> ignore(opt_cast_func typ msg.body func))

let dadd_signal_receiver connection ?sender ?destination ?path ?interface ?member ?(args=[]) func =
  with_running connection & fun running ->
    MSet.add running.signal_handlers
      ({ smr_sender = sender;
         smr_destination = destination;
         smr_path = path;
         smr_interface = interface;
         smr_member = member;
         smr_args = args },
       fun msg -> func msg.body)

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

let dispatch_message connection running message =
  DEBUG("received on %S: %s" running.guid (string_of_message message));

  (* First of all, pass the message through all filters *)
  MSet.iter (fun filter ->
               filter message;
               (* The connection may have crash during the execution
                  of the filter *)
               check_crash connection) running.filters;

  (* Now we do the specific dispatching *)
  match message with

    (* For method return and errors, we lookup at the reply
       waiters. If one is find then it get the reply, if none, then
       the reply is dropped. *)
    | { typ = `Method_return(reply_serial) } as message ->
        find_reply_handler running reply_serial
          (fun (handler, error_handler) ->
             try
               handler message
             with
                 exn when not (is_fatal exn) ->
                   (* Always notify the user of non-fatal
                      errors *)
                   error_handler exn)
          (fun _ ->
             DEBUG("reply to message with serial %ld dropped" reply_serial))

    | { typ = `Error(reply_serial, error_name) } ->
        let msg = get_error message in
        find_reply_handler running reply_serial
          (fun  (handler, error_handler) -> error_handler & OBus_error.make error_name msg)
          (fun _ ->
             DEBUG("error reply to message with serial %ld dropped because no reply was expected, \
                          the error is: %S: %S" reply_serial error_name msg))

    | { typ = `Signal _ } as message ->
        MSet.iter
          (fun (match_rule, handler) ->
             if signal_match match_rule message
             then begin
               handler message;
               check_crash connection
             end)
          running.signal_handlers

    | _ -> ()

(*  TODO: implement service proposal

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
*)

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

let rec dispatch_forever connection on_disconnect = match !connection with
  | Running running ->
      Lwt.bind (recv_one_message connection)
        (fun message ->
           begin
             try
               dispatch_message connection running message
             with
                 exn -> match !connection with
                   | Crashed _ -> ()
                   | Running running ->
                       remove_connection_of_guid_map running;
                       connection := Crashed exn
           end;
           dispatch_forever connection running.on_disconnect)
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
      outgoing = Lwt.return 0l;
      reply_handlers = Serial_map.empty;
      signal_handlers = MSet.make ();
(*      service_handlers = Interf_map.empty;*)
      filters = MSet.make ();
      guid = guid;
      name = None;
      shared = shared;
      on_disconnect = on_disconnect;
    } in
    Lwt.ignore_result & dispatch_forever connection on_disconnect;
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
