(*
 * oBus_error.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type name = string
type message = string
exception DBus of name * message

let () =
  Printexc.register_printer
    (function
       | DBus(name, message) -> Some(Printf.sprintf "%s: %s" name message)
       | _ -> None)

let failed = "org.freedesktop.DBus.Error.Failed"
let invalid_args = "org.freedesktop.DBus.Error.InvalidArgs"
let unknown_method = "org.freedesktop.DBus.Error.UnknownMethod"
let no_memory = "org.freedesktop.DBus.Error.NoMemory"
let no_reply = "org.freedesktop.DBus.Error.NoReply"
let ocaml = "org.ocamlcore.forge.obus.OCamlException"

let make name message = DBus(name, message)

let raise exn message = raise (make exn message)
let fail exn message = Lwt.fail (make exn message)
