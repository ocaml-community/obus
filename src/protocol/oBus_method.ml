(*
 * oBus_method.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(method)"

let call info proxy args =
  OBus_connection.method_call
    ~connection:(OBus_proxy.connection proxy)
    ~destination:(OBus_proxy.name proxy)
    ~path:(OBus_proxy.path proxy)
    ~interface:(OBus_member.Method.interface info)
    ~member:(OBus_member.Method.member info)
    ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
    ~o_args:(OBus_value.arg_types (OBus_member.Method.o_args info))
    args

let call_with_context info proxy args =
  let%lwt msg, result =
    OBus_connection.method_call_with_message
      ~connection:(OBus_proxy.connection proxy)
      ~destination:(OBus_proxy.name proxy)
      ~path:(OBus_proxy.path proxy)
      ~interface:(OBus_member.Method.interface info)
      ~member:(OBus_member.Method.member info)
      ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
      ~o_args:(OBus_value.arg_types (OBus_member.Method.o_args info))
      args
  in
  Lwt.return (OBus_context.make (OBus_proxy.connection proxy) msg, result)

let call_no_reply info proxy args =
  OBus_connection.method_call_no_reply
    ~connection:(OBus_proxy.connection proxy)
    ~destination:(OBus_proxy.name proxy)
    ~path:(OBus_proxy.path proxy)
    ~interface:(OBus_member.Method.interface info)
    ~member:(OBus_member.Method.member info)
    ~i_args:(OBus_value.arg_types (OBus_member.Method.i_args info))
    args
