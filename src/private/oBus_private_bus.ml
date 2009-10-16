(*
 * oBus_private_bus.ml
 * -------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_type.Perv
open Lwt

let destination = "org.freedesktop.DBus"
let path = ["org"; "freedesktop"; "DBus"]
let interface = "org.freedesktop.DBus"

let add_match connection =
  OBus_connection.method_call_no_reply connection ~member:"AddMatch" ~destination ~path ~interface <:obus_func< OBus_match.rule -> unit >>

let remove_match connection =
  OBus_connection.method_call_no_reply connection ~member:"RemoveMatch" ~destination ~path ~interface <:obus_func< OBus_match.rule -> unit >>

let get_name_owner connection name =
  try_lwt
    lwt n = OBus_connection.method_call connection ~member:"GetNameOwner" ~destination ~path ~interface <:obus_func< string -> string >> name in
    return (Some n)
  with _ ->
    return None
