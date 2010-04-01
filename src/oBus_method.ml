(*
 * oBus_method.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_message
open OBus_connection

let section = Lwt_log.Section.make "obus(method)"

(* +-----------------------------------------------------------------+
   | Calling methods                                                 |
   +-----------------------------------------------------------------+ *)

let call ~connection ?flags ?sender ?destination ~path ?interface ~member ty =
  OBus_type.make_func ty begin fun body ->
    let ty_reply = OBus_type.func_reply ty in
    lwt msg =
      send_message_with_reply
        connection
        (method_call ?flags ?sender ?destination ~path ?interface ~member body)
    in
    match msg with
      | { typ = Method_return _ } -> begin
          try
            return (OBus_type.cast_sequence ty_reply ~context:(connection, msg) msg.body)
          with OBus_type.Cast_failure _ as exn ->
            (* If not, check why the cast fail *)
            let expected_sig = OBus_type.type_sequence ty_reply
            and got_sig = OBus_value.type_of_sequence msg.body in
            if expected_sig = got_sig then
              (* If the signature match, this means that the user
                 defined a combinator raising a Cast_failure *)
              fail exn
            else
              (* In other case this means that the expected
                 signature is wrong *)
              fail
                (Failure (Printf.sprintf
                            "unexpected signature for reply of method %S on interface %S, expected: %S, got: %S"
                            member (match interface with Some i -> i | None -> "")
                            (OBus_value.string_of_signature expected_sig)
                            (OBus_value.string_of_signature got_sig)))
        end

      | { typ = Error(_, error_name) } ->
          fail
            (OBus_error.make
               error_name
               (match msg.body with
                  | OBus_value.Basic(OBus_value.String x) :: _ -> x
                  | _ -> ""))

      | _ ->
          assert false
  end

let call_no_reply ~connection ?flags ?sender ?destination ~path ?interface ~member ty =
  OBus_type.make_func ty begin fun body ->
    send_message connection (method_call ?flags ?sender ?destination ~path ?interface ~member body)
  end

let dyn_call ~connection ?flags ?sender ?destination ~path ?interface ~member body =
  lwt { body = x } =
    send_message_with_reply connection (method_call ?flags ?sender ?destination ~path ?interface ~member body)
  in
  return x

let dyn_call_no_reply ~connection ?flags ?sender ?destination ~path ?interface ~member body =
  send_message connection (method_call ?flags ?sender ?destination ~path ?interface ~member body)

(* +-----------------------------------------------------------------+
   | Sending replies                                                 |
   +-----------------------------------------------------------------+ *)

let dyn_return ~context:(connection, { sender = sender; serial = serial }) body =
  send_message connection { destination = sender;
                            sender = None;
                            flags = { no_reply_expected = true; no_auto_start = true };
                            serial = 0l;
                            typ = Method_return serial;
                            body = body }

let return ~context typ v =
  dyn_return ~context (OBus_type.make_sequence typ v)

let error ~context:(connection, { sender = sender; serial = serial }) ~name ~message =
  send_message connection { destination = sender;
                            sender = None;
                            flags = { no_reply_expected = true; no_auto_start = true };
                            serial = 0l;
                            typ = Error(serial, name);
                            body = [OBus_value.sstring message] }

let fail ~context exn =
  match OBus_error.cast exn with
    | Some(name, message) ->
        error ~context ~name ~message
    | None ->
        lwt () = Lwt_log.error ~section ~exn "sending an unregistred ocaml exception as a D-Bus error" in
        error ~context ~name:"ocaml.Exception" ~message:(Printexc.to_string exn)
