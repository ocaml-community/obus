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

exception Invalid_reply = OBus_private_method.Invalid_reply

let call info proxy args =
  OBus_private_method.call
    ~connection:(OBus_proxy.connection proxy)
    ~destination:(OBus_proxy.name proxy)
    ~path:(OBus_proxy.path proxy)
    ~interface:(OBus_member.Method.interface info)
    ~member:(OBus_member.Method.member info)
    ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
    ~o_args:(OBus_value.arg_types (OBus_member.Method.o_args info))
    args

let call_with_context info proxy args =
  OBus_private_method.call_with_context
    ~connection:(OBus_proxy.connection proxy)
    ~destination:(OBus_proxy.name proxy)
    ~path:(OBus_proxy.path proxy)
    ~interface:(OBus_member.Method.interface info)
    ~member:(OBus_member.Method.member info)
    ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
    ~o_args:(OBus_value.arg_types (OBus_member.Method.o_args info))
    args

let call_no_reply info proxy args =
  OBus_private_method.call_no_reply
    ~connection:(OBus_proxy.connection proxy)
    ~destination:(OBus_proxy.name proxy)
    ~path:(OBus_proxy.path proxy)
    ~interface:(OBus_member.Method.interface info)
    ~member:(OBus_member.Method.member info)
    ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
    args

(* +-----------------------------------------------------------------+
   | Sending replies                                                 |
   +-----------------------------------------------------------------+ *)

let return context x =
  lwt () = OBus_private_connection.send_reply context x in
  Lwt.return `Replied

let fail context exn =
  lwt () = OBus_private_connection.send_error context exn in
  Lwt.return `Replied
