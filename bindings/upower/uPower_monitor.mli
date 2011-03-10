(*
 * uPower_monitor.mli
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Properties monitoring *)

val monitor : OBus_property.monitor
  (** Monitor for properties of upower interfaces. *)
