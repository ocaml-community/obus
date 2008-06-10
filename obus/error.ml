(*
 * error.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open ThreadImplem

exception DBus of string * string

open Printf

type error_maker = string -> string -> exn option
type error_unmaker = exn -> (string * string) option

let error_makers = Protected.make []
let error_unmakers = Protected.make []

let make_error name msg =
  match Util.find_map (fun maker -> maker name msg) (Protected.get error_makers) with
    | Some exn -> exn
    | None -> DBus(name, msg)

let unmake_error = function
  | DBus(name, msg) -> Some(name, msg)
  | exn -> Util.find_map (fun maker -> maker exn) (Protected.get error_unmakers)

let register_maker f = Protected.update (fun l -> f :: l) error_makers
let register_unmaker f = Protected.update (fun l -> f :: l) error_unmakers
