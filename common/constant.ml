(*
 * constant.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let max_array_size = 1 lsl 26
let max_message_size = 1 lsl 27
let protocol_version = 1
let default_system_bus_address = "unix:path=/var/run/dbus/system_bus_socket"
