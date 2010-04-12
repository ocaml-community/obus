(*
 * oBus_context.mli
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Message contexts *)

type 'a t = 'a OBus_private_connection.context
  (** Type of a context. In case of a method call, ['a] is the type of
      the method reply. *)

val make :
  connection : OBus_connection.t ->
  message : OBus_message.t -> unit t
  (** [make ~connection ~message] creates a new context. [message] is
      the received message and [connection] is the connection from
      which it comes. *)

val make_with_arguments :
  connection : OBus_connection.t ->
  message : OBus_message.t ->
  arguments : 'a OBus_value.arguments -> 'a t
  (** [make_with_arguments ~connection ~message ~arguments] is the
      same as {!make} except that the conext contains argument
      informations. It is only meaningful if [message] is a method
      return, in which case [arguments] describe arguments of the
      reply. *)

val connection : 'a t -> OBus_connection.t
  (** Returns the connection part of a context *)

val message : 'a t -> OBus_message.t
  (** Returns the message part of a context *)

val arguments : 'a t -> 'a OBus_value.arguments
  (** Returns the arguments of a context *)

(** {6 Replies} *)

val replied : 'a t -> bool
  (** If the given context contains a method call, [replied ctx]
      returns whether the reply have been sent to the caller. It is
      initially [false].

      The goal of {!replied} is to prevent from sending multiple
      replies to the caller. *)

val set_replied : 'a t -> unit
  (** [set_replied ctx] sets the replied flag to [true]. *)

(** {6 Helpers} *)

val sender : 'a t -> OBus_peer.t
  (** [sender context] returns the peer who sends the message *)
