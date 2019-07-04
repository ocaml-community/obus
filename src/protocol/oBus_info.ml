(*
 * oBus_info.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(info)"

let version = OBus_config.version

let protocol_version = 1
let max_name_length = OBus_protocol.max_name_length
let max_message_size = OBus_protocol.max_message_size

let read_uuid_file file =
  try%lwt
    let%lwt line = Lwt_io.with_file ~mode:Lwt_io.input file Lwt_io.read_line in
    Lwt.return (OBus_uuid.of_string line)
  with exn ->
    ignore (Lwt_log.error_f ~section ~exn "failed to read the local machine uuid from file %S" file);
    Lwt.fail exn

let machine_uuid = lazy(
  try%lwt
    read_uuid_file OBus_config.machine_uuid_file
  with exn ->
    try%lwt
      read_uuid_file "/etc/machine-id"
    with _ ->
      Lwt.fail exn
)
