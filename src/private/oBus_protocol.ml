(*
 * oBus_protocol.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Protocol parameters *)

let max_type_recursion_depth = 32
let max_name_length = 255
let max_array_size = 1 lsl 26
let max_message_size = 1 lsl 27

let bus_name = "org.freedesktop.DBus"
let bus_path = ["org"; "freedesktop"; "DBus"]
let bus_interface = "org.freedesktop.DBus"
