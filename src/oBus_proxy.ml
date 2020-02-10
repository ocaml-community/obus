(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(proxy)"

open OBus_peer
open OBus_message

type t = {
  peer : OBus_peer.t;
  path : OBus_path.t;
}

let compare = Pervasives.compare

let make ~peer ~path = { peer = peer; path = path }

let peer proxy = proxy.peer
let path proxy = proxy.path
let connection proxy = proxy.peer.connection
let name proxy = proxy.peer.name

type proxy = t

module type Private = sig
  type t = private proxy
  external of_proxy : proxy -> t = "%identity"
  external to_proxy : t -> proxy = "%identity"
end

module Private =
struct
  type t = proxy
  external of_proxy : proxy -> t = "%identity"
  external to_proxy : t -> proxy = "%identity"
end

(* +-----------------------------------------------------------------+
   | Method calls                                                    |
   +-----------------------------------------------------------------+ *)

let call proxy ~interface ~member ~i_args ~o_args args =
  OBus_connection.method_call
    ~connection:proxy.peer.connection
    ~destination:proxy.peer.name
    ~path:proxy.path
    ~interface
    ~member
    ~i_args
    ~o_args
    args

let call_with_context proxy ~interface ~member ~i_args ~o_args args =
  let%lwt msg, result =
    OBus_connection.method_call_with_message
      ~connection:proxy.peer.connection
      ~destination:proxy.peer.name
      ~path:proxy.path
      ~interface
      ~member
      ~i_args
      ~o_args
      args
  in
  Lwt.return (OBus_context.make proxy.peer.connection msg, result)

let call_no_reply proxy ~interface ~member ~i_args args =
  OBus_connection.method_call_no_reply
    ~connection:proxy.peer.connection
    ~destination:proxy.peer.name
    ~path:proxy.path
    ~interface
    ~member
    ~i_args
    args

(* +-----------------------------------------------------------------+
   | Introspection                                                   |
   +-----------------------------------------------------------------+ *)

let introspect proxy =
  let%lwt str =
    call proxy ~interface:"org.freedesktop.DBus.Introspectable" ~member:"Introspect"
      ~i_args:OBus_value.C.seq0
      ~o_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
      ()
  in
  try
    Lwt.return (OBus_introspect.input (Xmlm.make_input ~strip:true (`String(0, str))))
  with Xmlm.Error((line, column), err) ->
    Lwt.fail (Failure(Printf.sprintf "OBus_proxy.introspect: invalid document, at line %d: %s" line (Xmlm.error_message err)))
