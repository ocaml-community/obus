(*
 * constant.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

val max_array_size : int
  (** Maximum size of an array *)

val max_message_size : int
  (** Maximum size of a message *)

val protocol_version : int
  (** The protocol version implemented by obus *)

val default_system_bus_address : string
  (** Address of the system bus *)
