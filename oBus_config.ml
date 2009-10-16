(*
 * oBus_config.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* This files is aimed to be edited by packagers *)

(* Localtion of the machine id file. It is generally created by
   libdbus: *)
let machine_uuid_file = "/var/lib/dbus/machine-id"

(* The native byte order, which depends on the architecture on which
   the library will be used. Set it either to [`little_endian] or
   [`big_endian]: *)
let native_byte_order = `little_endian
