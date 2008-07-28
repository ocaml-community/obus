(*
 * oBus_signal.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** High-level handling of signals *)

type 'a t
  (** Type of a signal, ['a] is the type of an handler *)

val attach : connection -> 'a t -> 'a -> unit
  (** Attach a function to a signal. The function will be called every
      time the signal is received *)

val emit : ?destination:name -> 'a t -> 'a
  (** emit a signal *)

val make : interface:name -> member:name -> ('a, unit, unit, [> `func ]) OBus_conv.t -> 'a t
  (** Create a signal *)
