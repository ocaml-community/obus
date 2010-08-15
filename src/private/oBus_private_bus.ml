(*
 * oBus_private_bus.ml
 * -------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_value

let interface = "org.freedesktop.DBus"
let destination = "org.freedesktop.DBus"
let path = ["org"; "freedesktop"; "DBus"]

let add_match connection match_rule =
  OBus_private_method.call_no_reply
    ~connection
    ~destination
    ~path
    ~interface
    ~member:"AddMatch"
    ~i_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
    match_rule

let remove_match connection match_rule =
  OBus_private_method.call_no_reply
    ~connection
    ~destination
    ~path
    ~interface
    ~member:"RemoveMatch"
    ~i_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
    match_rule

let get_name_owner connection name =
  try_lwt
    OBus_private_method.call
      ~connection
      ~destination
      ~path
      ~interface
      ~member:"GetNameOwner"
      ~i_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
      ~o_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
      name
  with _ ->
    return ""
