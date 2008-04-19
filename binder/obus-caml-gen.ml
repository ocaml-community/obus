(*
 * obus-caml-gen.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module M = Binder.MakeGen(Caml.Make(struct let rules = Caml.Rules.default end))

let _ = M.main ()
