(*
 * oBus_signal.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** High-level handling of signals *)

(** This module let you to connect callback function to signal and to
    emit signal.

    Note that when the connection used as backend is a message bus,
    the matching rules will be automatically added so you do not have
    to do it. *)

type 'a t
  (** Type of a signal, ['a] is the type of a callback function *)

type id
  (** Id of a connected callback function *)

val remove : id -> unit

val connect : 'a t -> 'a -> id
  (** [connect signal func] connect [func] to [signal].

      Note: multiple callback function can be connected to the same
      signal. *)

val disconnect : id -> unit
  (** Remove a previously registred callback function. It do nothing
      if the callback function has already been disconnected *)

val emit : ?destination:string -> path:OBus_path.t -> 'a t -> 'a
  (** emit a signal *)

val make : interface:string -> member:string -> ('a, unit, unit) OBus_comb.func -> 'a t
  (** [make interface member typ] create a signal *)
