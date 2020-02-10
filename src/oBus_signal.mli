(*
 * oBus_signal.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus signals *)

(** {6 Emitting signals} *)

val emit : 'a OBus_member.Signal.t -> 'b OBus_object.t -> ?peer : OBus_peer.t -> 'a -> unit Lwt.t
  (** [emit signal obj ?peer args] emits [signal] from [obj]. The
      destinations of the signal are selected as follow:

      - if [peer] is provided, then the message is sent only to it
      - otherwise, if the the object has an owner, it is sent to the owner,
      - otherwise, the message is broadcasted on all the connections [obj]
        is exported on.
  *)

(** {6 Receving signals} *)

type 'a t
  (** Type of a signal descriptor. A signal descriptor represent the
      source of a signal and describes how the value should be
      transformed. *)

val make : 'a OBus_member.Signal.t -> OBus_proxy.t -> 'a t
  (** [make signal proxy] creates a signal descriptor. *)

val make_any : 'a OBus_member.Signal.t -> OBus_peer.t -> (OBus_proxy.t * 'a) t
  (** [make_any signal peer] creates a signal descriptor for receiving
      signals from any object of [peer]. *)

val connect : ?switch : Lwt_switch.t -> 'a t -> 'a React.event Lwt.t
  (** [connect ?switch sd] connects the signal descriptor [sd] and
      returns the event which occurs when the given D-Bus signal is
      received. *)

(** {6 Signals transformations and parameters} *)

val map_event : ((OBus_context.t * 'a) React.event -> (OBus_context.t * 'b) React.event) -> 'a t -> 'b t
  (** [map_event f sd] transforms with [f] the event that is created
      when [sd] is connected. *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Simplified version of {!map_event}. *)

val map_with_context : (OBus_context.t -> 'a -> 'b) -> 'a t -> 'b t
  (** Same as {!map} but the mapping function also receive the
      context. *)

val with_context : 'a t -> (OBus_context.t * 'a) t
  (** @return a signal descriptor that returns contexts in which
      signals are received. *)

val with_filters : OBus_match.arguments -> 'a t -> 'a t
  (** [with_filters filters sd] is the signal descriptor [sd] with the
      given list of argument filters. When connected, obus will add
      this filters to the matching rule send to the message bus, so
      the bus can use them to drop messages that do not match these
      filters.

      The goal of argument filters is to reduce the number of messages
      received, and so to reduce the number of wakeup of the
      program.

      Note that match rule management must be activated for filters to
      take effect (see {!with_match_rule}).  *)

val with_match_rule : bool -> 'a t -> 'a t
  (** [with_match_rule state sd] enables or disables the automatic
      management of matching rules. If the endpoint of the underlying
      connection is a message bus it defaults to [true], otherwise it
      default to [false]. *)
