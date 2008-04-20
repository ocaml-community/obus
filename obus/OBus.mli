(*
 * OBus.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** {6 Connection} *)

type bus
  (** Abstract type for a message bus *)

exception Connection_failed

val session : unit -> bus
  (** [session ()] establish a connection with the session message bus *)

val system : unit -> bus
  (** [connect ()] establish a connection with the system message bus *)

val connect_to_bus : string -> bus
  (** [connect_to_bus address] establish a connection with the bus
      which address is described by [address] *)

(** {6 DBus values and types} *)

module Types : Types.S
module Values : Values.S

(** {6 Low level operations} *)

module LowLevel : LowLevel.S with type bus = bus
                             and module T = Types
                             and module V = Values
