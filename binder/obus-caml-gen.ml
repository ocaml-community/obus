(*
 * obus-caml-gen.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

module M = Binder.MakeGen(Caml)

let _ = M.main ()
