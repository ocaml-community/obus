(*
 * printInterf.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

val print : bool -> out_channel -> Introspect.module_tree -> unit
  (** [print internal formatter node] *)
