(*
 * oBus_info.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

include Version

let protocol_version = 1
let max_name_length = Constant.max_name_length
let max_message_size = Constant.max_message_size

let verbose = Log.verbose_enable
let debug = Log.debug_enable
let dump = Log.dump_enable

(* This location depends on where libdbus is installed *)
let machine_uuid_file = "/var/lib/dbus/machine-id"

let machine_uuid = lazy
  (* try to get the uuid with the dbus-uuidgen program, so we do not
     have to care about where the uuid file is located *)
  (perform
     line <-- catch
       (fun _ -> Util.with_process_in "dbus-uuidgen" [|"dbus-uuidgen"; "--get"|] Lwt_chan.input_line)
       (fun exn ->
          Log.log "dbus-uuidgen failed: %s" (Util.string_of_exn exn);
          (* Try reading the file *)
          catch
            (fun _ -> Util.with_open_in machine_uuid_file Lwt_chan.input_line)
            (fun exn ->
               Log.error "failed to read the local machine uuid file (%s): %s"
                 machine_uuid_file (Util.string_of_exn exn);
               fail exn));
     try
       return (OBus_uuid.of_string line)
     with
         exn -> fail exn)
