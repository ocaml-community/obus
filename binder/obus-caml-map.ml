(*
 * obus-caml-map.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module M = Binder.MakeMap(Caml.Make(struct let rules = Caml.Rules.default end))

let _ = M.main ()
