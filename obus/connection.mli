(*
 * connection.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Interface for dbus connection *)

type t
  (** Abstract type for a connection *)

(** {6 Creation} *)

val of_transport : Transport.t -> t option
  (** [of_transport transport] create a dbus connection over the given
      transport *)

val of_address : [> Address.t ] list -> t option
  (** [of_address addresses] shorthand for obtaining transport and
      doing [of_transport] *)

(** {6 Sending messages} *)

val send_message : t -> Message.t -> Message.serial
  (** [send_message connection message] send a message over a DBus
      connection *)

val wait_reply : t -> Message.serial -> Message.t
  (** [wait_reply connection serial] wait for a message with this
      serial come from the connection *)

(** For auto-generated interface.

    Do not use this, this is unsafe! *)
module Internal : Internal.S
  with type connection = t
