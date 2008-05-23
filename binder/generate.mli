(*
 * generate.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types

type ('a, 'b, 'v) args = ('a, 'v, 'b, 'v list -> 'v) Seq.t
type dependency = typ * typ

type 'v rule

val rule : typ -> typ -> (dependency, 'a, 'v) args -> dependency list -> 'a -> 'v rule

val generate : ?trace:bool -> 'v rule list -> typ -> typ -> 'v option
