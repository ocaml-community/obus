(*
 * request-name.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus

let name = "org.foo.Bar"

let _ =
  let bus = Bus.session () in
    match DBus.request_name bus name [ `Replace_existing; `Do_not_queue ] with
      | `Primary_owner ->
          Printf.printf "i successfully got the name '%s' !!\n" name
      | _ ->
          Printf.printf ":(\n"
