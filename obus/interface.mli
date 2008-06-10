(*
 * interface.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of dbus interfaces *)

type name = string

type 'a t
  (** Abstract type representing an interface *)

val name : 'a t -> name
  (** [name interface] get the name of an interface *)

(**/**)

val intern_make : 'a -> name -> 'a t
