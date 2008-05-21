(*
 * info.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

val native_byte_order : Header.byte_order
  (** byte order of the current architecture *)

val protocol_version : int
  (** The version of the protocol implemented by the library *)
