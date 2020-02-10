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
    message. *)

type t
  (** Type of a context. *)

(** {6 Creation} *)

val make : connection : OBus_connection.t -> message : OBus_message.t -> t
  (** Creates a context from the given connection and message *)

(** {6 Retreival} *)

val get : unit -> t
  (** In a method call handler, this returns the context of the method
      call. *)

val key : t Lwt.key
  (** The key used for storing the context. *)

(** {6 Projections} *)

val connection : t -> OBus_connection.t
  (** Returns the connection part of a context *)

val sender : t -> OBus_peer.t
  (** [sender context] returns the peer who sends the message *)

val destination : t -> OBus_peer.t
  (** [destinatino context] returns the peer to which the message was
      sent *)

val flags : t -> OBus_message.flags
  (** [flags context] returns the flags of the message that was
      received *)

val serial : t -> OBus_message.serial
  (** Returns the serial of the message *)
