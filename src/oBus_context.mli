(*
 * oBus_context.mli
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Message contexts *)

(** {6 Types} *)

(** A context contains information about the reception of a
    message. It may also be used to send a reply to a method call *)

type 'a t = 'a OBus_private_connection.message_context
    (** Type of a context. In case of a method call, [`a] is a value
        of the type of the method reply, otherwise, it is
        meaningless. *)

type void = OBus_private_connection.void
  (** Empty type *)

(** {6 Creation} *)

val make : connection : OBus_connection.t -> message : OBus_message.t -> void t
  (** Creates a context from the given connection and message *)

val make_with_reply : connection : OBus_connection.t -> message : OBus_message.t -> OBus_value.V.sequence t
  (** Creates a context that can be used to reply to a method call *)

(** {6 Projections} *)

val connection : 'a t -> OBus_connection.t
  (** Returns the connection part of a context *)

val sender : 'a t -> OBus_peer.t
  (** [sender context] returns the peer who sends the message *)

val destination : 'a t -> OBus_peer.t
  (** [destinatino context] returns the peer to which the message were
      sent *)

val flags : 'a t -> OBus_message.flags
  (** [flags context] returns the flags of the message that was
      received *)

val serial : 'a t -> OBus_message.serial
  (** Returns the serial of the message *)

(** {6 Replies} *)

val map : ('b -> 'a) -> 'a t -> 'b t
  (** Map the type of reply to a method call *)

val make_body : 'a t -> 'a -> OBus_value.V.sequence
  (** [make_body ctx value] creates the body of a reply to a method
      call *)
