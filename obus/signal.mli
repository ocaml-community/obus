(*
 * signal.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** High-level handling of signals *)

type ('a, 'b) set
  (** Represent a set of signals of type ['b], for dbus object of type
      ['a]. *)

type ('a, 'b) handler = 'a Proxy.t -> 'b -> bool
  (** Type for a function receiving signals of type ['b] from an
      object of type ['a].

      Note: several handler can receive the same signal. Once a
      handler consider the signal as been handled it should return
      [true], so other handler will not be used on this signal. *)

val register : Connection.t -> ('a, 'b) set -> ('a, 'b) handler -> unit
  (** [register connection signals handler] add a handler for the given set of signal.

      Note: if [connection] is a connection to a message bus, then
      just calling this function will not make the application to
      receive these signals. You may use {!bus_register} for that or
      manually call [DBus.add_match] with appropriate arguments. *)

val bus_register : Bus.t -> ('a, 'b) set -> ('a, 'b) handler -> unit
  (** [bus_register bus signals handler] same as {!register} but also
      tell the message bus that we want it to route these signals to
      us *)

(**/**)

val make_set : 'a Interface.t -> (Connection.t -> ('a, 'b) handler -> bool Connection.reader) -> ('a, 'b) set
