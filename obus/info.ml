(*
 * info.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* TODO : make something better *)
let native_byte_order = Wire.Little_endian

let protocol_version = Constant.protocol_version
