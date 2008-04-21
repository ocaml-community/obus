(*
 * OBus.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** OBus main module.

    Note: this module is destined to be used with automatically
    generated dbus interfaces.

    You can not send arbitrary data over the bus, if this is what you
    want, look at the lowlevel library.
*)

(** {6 Connection} *)

module Bus : sig
  type t

  exception Connection_failed of string

  val session : unit -> t
    (** [session ()] establish a connection with the session message bus *)

  val system : unit -> t
    (** [connect ()] establish a connection with the system message bus *)

  val connect : string -> t
    (** [connect address] establish a connection with the bus which
        address is described by [address] *)

  val dispatch : bus -> unit
    (** [dispatch bus] read all pending call *)

  val fd : t -> Unix.file_descr
    (** [fd bus] get the file descriptor associated with the bus (for
        select) *)
end

(** {6 DBus values and types} *)

module Types : Types.S
  (** Representation of dbus types *)

module Values : Values.S
  (** Representation of dbus values *)
  with module T = Types

(** {6 Signals} *)

module Signal : sig
  type 'a t

  type id
    (** Identifier for a signal handler *)

  val register : Bus.t -> 'a t -> ('a -> unit) -> id
    (** [register bus signal handler] add a signal handler. Each time
        the signal [signal] came from the bus [handler] will be called
        with the signal arguments as parameters. *)

  val unregister : Bus.t -> id -> unit
    (** [unregister bus id] delete a handler *)

  val clear : Bus.t -> 'a t -> unit
    (** [clear bus signal] clear all signal handler for the given
        signal *)
end
