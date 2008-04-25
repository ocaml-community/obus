(*
 * protected.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a t = 'a ref

let make x = ref x
let get = ( ! )
let set = ( := )
let update f x = x := f !x
let safe_update f x = x := f !x
let process f x = let y, z = f !x in x := z; y
let safe_process = process
