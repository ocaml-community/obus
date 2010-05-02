(*
 * oBus_context.mli
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Message contexts *)

(** A context contains information about the reception of a
    message. It may also be used to send a reply to a method call *)

type 'a t = 'a OBus_private_connection.context
    (** Type of a context. In case of a method call, [`a] is a value
        of the type of the method reply, otherwise, it is
        meaningless. *)

type void = OBus_private_connection.void
  (** Empty type *)

val make : connection : OBus_connection.t -> message : OBus_message.t -> void t
  (** Creates a context from the given connection and message *)

val make_with_reply : connection : OBus_connection.t -> message : OBus_message.t -> OBus_value.V.sequence t
  (** Creates a context that can be used to reply to a method call *)

val connection : 'a t -> OBus_connection.t
  (** Returns the connection part of a context *)

val message : 'a t -> OBus_message.t
  (** Returns the message part of a context *)

val sender : 'a t -> OBus_peer.t
  (** [sender context] returns the peer who sends the message *)

val destination : 'a t -> OBus_peer.t
  (** [destinatino context] returns the peer to which the message were
      sent *)

val map : ('b -> 'a) -> 'a t -> 'b t
  (** Map the type of reply to a method call *)

val replied : 'a t -> bool
  (** [replied context] returns whether the reply have been sent to
      the caller. It is initially [false].

      The goal of {!replied} is to prevent from sending multiple
      replies to the caller. *)

val set_replied : 'a t -> unit
  (** [set_replied ctx] sets the replied flag to [true]. *)

val make_body : 'a t -> 'a -> OBus_value.V.sequence
  (** [make_body ctx value] creates the body of a reply to a method
      call *)
