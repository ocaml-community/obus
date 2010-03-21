(*
 * oBus_info.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(info)"

open Lwt

let version = OBus_version.version

let protocol_version = 1
let max_name_length = OBus_constant.max_name_length
let max_message_size = OBus_constant.max_message_size

let machine_uuid = lazy(
  try_lwt
    lwt line = Lwt_io.with_file ~mode:Lwt_io.input OBus_config.machine_uuid_file Lwt_io.read_line in
    return (OBus_uuid.of_string line)
  with exn ->
    lwt () =
      Lwt_log.error_f ~section "failed to read the local machine uuid from file %S: %s"
        OBus_config.machine_uuid_file (Printexc.to_string exn)
    in
    fail exn
)
