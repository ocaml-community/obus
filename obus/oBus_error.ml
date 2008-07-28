(*
 * oBus_error.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type name = string
type message = string
exception DBus of name * message
exception Failed of message
exception Unknown_method of message
exception Out_of_memory of message

open Printf

let errors = ref
  [ "org.freedesktop.Error.Failed",
    ((fun msg -> Failed msg),
     (function
        | Failed msg -> Some msg
        | _ -> None));
    "org.freedesktop.Error.UnknownMethod",
    ((fun msg -> Unknown_method msg),
     (function
        | Unknown_method msg -> Some msg
        | _ -> None));
    "org.freedesktop.Error.OOM",
    ((fun msg -> Out_of_memory msg),
     (function
        | Out_of_memory msg -> Some msg
        | _ -> None)) ]

let make name msg =
  match Util.assoc name !errors with
    | Some (maker, unmaker) -> maker msg
    | None -> DBus(name, msg)

let unmake = function
  | DBus(name, msg) -> Some(name, msg)
  | exn -> Util.find_map (fun (name, (maker, unmaker)) ->
                            Util.Maybe.bind
                              (unmaker exn)
                              (fun msg -> Some(name, msg))) !errors

let register name maker unmaker = errors := (name, (maker, unmaker)) :: !errors
