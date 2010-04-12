(*
 * oBus_method.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(method)"

open Lwt
open OBus_message

(* +-----------------------------------------------------------------+
   | Calling methods                                                 |
   +-----------------------------------------------------------------+ *)

exception Invalid_reply of string

let () = OBus_private_method.invalid_reply := (fun msg -> Invalid_reply msg)

let call info proxy args =
  OBus_private_method.call
    ~connection:(OBus_proxy.connection proxy)
    ?destination:(OBus_proxy.name proxy)
    ~path:(OBus_proxy.path proxy)
    ~interface:(OBus_member.Method.interface info)
    ~member:(OBus_member.Method.member info)
    ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
    ~o_args:(OBus_value.arg_types (OBus_member.Method.o_args info))
    args

let call_with_context info proxy args =
  OBus_private_method.call_with_context
    ~connection:(OBus_proxy.connection proxy)
    ?destination:(OBus_proxy.name proxy)
    ~path:(OBus_proxy.path proxy)
    ~interface:(OBus_member.Method.interface info)
    ~member:(OBus_member.Method.member info)
    ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
    ~o_args:(OBus_value.arg_types (OBus_member.Method.o_args info))
    args

let call_no_reply info proxy args =
  OBus_private_method.call_no_reply
    ~connection:(OBus_proxy.connection proxy)
    ?destination:(OBus_proxy.name proxy)
    ~path:(OBus_proxy.path proxy)
    ~interface:(OBus_member.Method.interface info)
    ~member:(OBus_member.Method.member info)
    ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
    args

(* +-----------------------------------------------------------------+
   | Sending replies                                                 |
   +-----------------------------------------------------------------+ *)

let return ~context x =
  if OBus_context.replied context then
    return ()
  else begin
    OBus_context.set_replied context;
    let msg = OBus_context.message context in
    OBus_connection.send_message (OBus_context.connection context) {
      destination = msg.sender;
      sender = None;
      flags = { no_reply_expected = true; no_auto_start = true };
      serial = 0l;
      typ = Method_return msg.serial;
      body = OBus_value.C.make_sequence (OBus_value.arg_types (OBus_context.arguments context)) x;
    }
  end

let fail_by_name ~context name error_message =
  if OBus_context.replied context then
    Lwt.return ()
  else begin
    OBus_context.set_replied context;
    let msg = OBus_context.message context in
    OBus_connection.send_message (OBus_context.connection context) {
      destination = msg.sender;
      sender = None;
      flags = { no_reply_expected = true; no_auto_start = true };
      serial = 0l;
      typ = Error(msg.serial, name);
      body = [OBus_value.V.basic_string error_message];
    }
  end

let fail ~context exn error_message =
  fail_by_name ~context (OBus_error.name_of_exn exn) error_message
