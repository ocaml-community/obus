(*
 * info.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let native_byte_order = match Wire.native_byte_order () with
  | 0 -> Header.Little_endian
  | 1 -> Header.Big_endian
  | _ -> assert false

let protocol_version = Constant.protocol_version
