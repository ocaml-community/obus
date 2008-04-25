(*
 * protected.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a procted = 'a ref

let make x = x
let get = ( ! )
let set = ( := )
let update x f = x := f x
let safe_update x f = x := f x
let process x f = let y, z = f !x in x := z; y
let safe_process = update
