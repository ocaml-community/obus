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
let max_array_size = Constant.max_array_size
let max_message_size = Constant.max_message_size

let verbose, debug =
  try
    match String.lowercase (Sys.getenv "OBUSLOG") with
      | "debug" -> (true, true)
      | _ -> (true, false)
  with
      Not_found -> (false, false)
