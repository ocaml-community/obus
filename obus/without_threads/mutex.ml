(*
 * mutex.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type t = unit
let create _ = ()
let lock _ = ()
let try_lock _ = true
let unlock _ = ()
