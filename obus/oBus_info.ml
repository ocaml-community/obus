(*
 * oBus_info.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let max_name_length = 255

let max_array_size = 1 lsl 26
let max_message_size = 1 lsl 27
let protocol_version = 1

let verbose, debug =
  try
    match String.lowercase (Sys.getenv "OBUSLOG") with
      | "debug" -> (true, true)
      | _ -> (true, false)
  with
      Not_found -> (false, false)
