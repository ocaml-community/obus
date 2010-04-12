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

val emit : 'a OBus_member.Signal.t -> 'a OBus_object.t -> ?peer : OBus_peer.t -> 'a -> unit Lwt.t
  (** [emit signal obj args] emit [signal] from [obj]. The
      destinations of the signal are selected as follow:

      {ol
        {- if [peer] is provided, then the message is sent only to it,}
        {- otherwise, if the the object has an owner, it is sent to the owner,}
        {- otherwise, the message is boradcasted on all the connection [obj] is exported.}
      }
  *)

(** {6 Receving signals} *)

type 'a t
  (** Type of a signal receiver, which occurs with values of type ['a] *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f signal] maps the values returned by [signal] with [f] *)

val map_with_context : (unit OBus_context.t -> 'a -> 'b) -> 'a t -> 'b t
  (** [map_with_context f signal] maps the values returned by [signal]
      with [f], and also pass to [f] the context. *)

val event : 'a t -> 'a React.event
  (** The event which occurs each time the signal is received. *)

val event_with_context : 'a t -> (unit OBus_context.t * 'a) React.event
  (** Same as {!event} but adds the context to events *)

val connect : 'a OBus_member.Signal.t -> OBus_proxy.t -> 'a t
  (** [connect signal proxy] connects to signals emited by [proxy]. *)

val disconnect : 'a t -> unit
  (** [disconnect signal] stops receiving the given signal.

      Note that [disconnect] is automatically called when [event
      signal] is garbage collected *)

(** {8 Signals configuration} *)

val set_filters : 'a t -> (int * OBus_match.argument_filter) list -> unit
  (** Sets the list of argument filters for the given signal. This
      means that the message bus will filter signals that must be
      delivered to the current running program.

      The goal of argument filters is to reduce the number of messages
      received, and so to reduce the number of wakeup of the
      program. *)

val auto_match_rule : 'a t -> bool
  (** Returns whether automatic match rules management is enabled for
      this signal. It is always activated by default. *)

val set_auto_match_rule : 'a t -> bool -> unit
  (** Enable/disable the automatic management of matching rules. If
      you disable it, it is then up to you to add the correct rule on
      the bus by using {!OBus_bus.add_match}. *)

val init : ?filters : (int * OBus_match.argument_filter) list -> ?auto_match_rule : bool -> 'a t -> 'a React.event
  (** [init ?filters ?auto_match_rule ()] is an helper to sets
      signals parameters. Instead of
      {[
        let signal = Foo.bar proxy in
        OBus_signal.set_auto_match_rule signal false;
        OBus_signal.set_filters signal filters;
        let x = React.E.map (...) (OBus_signal.event signal)
      ]}
      you can write:
      {[
        let x = React.E.map (...) (OBus_signal.init ~filters ~auto_match_rule:false  (Foo.bar proxy))
      ]}
  *)
