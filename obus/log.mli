(*
 * log.mli
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Print information, according to the value of OBUSLOG *)

val verbose : bool
val authentification : bool
val transport : bool
val connection : bool
