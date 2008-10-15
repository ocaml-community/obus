(*
 * oBus_info.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type byte_order = Little_endian | Big_endian
let native_byte_order = Little_endian

let version = OBUS_VERSION

let max_name_length = 255

let max_array_size = 1 lsl 26
let max_message_size = 1 lsl 27
let protocol_version = 1

let verbose = Log.verbose_enable
let debug = Log.debug_enable
let dump = Log.dump_enable

(* This location depends on where libdbus is installed *)
let machine_uuid_file = "/var/lib/dbus/machine-id"

let machine_uuid = lazy(
  (* try to get the uuid with the dbus-uuidgen program, so we do not
     have to care about where the uuid file is located *)
  try Util.with_process_in "dbus-uuidgen --get" input_line with
      exn ->
        Log.log "dbus-uuidgen failed: %s" (Util.string_of_exn exn);
        (* Try reading the file *)
        try Util.with_open_in machine_uuid_file input_line with
            _ ->
              Log.error "failed to read the local machine uuid file (%s): %s"
                machine_uuid_file (Util.string_of_exn exn);
              raise exn
)
