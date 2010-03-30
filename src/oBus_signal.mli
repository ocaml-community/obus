(*
 * oBus_signal.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Client-side signals *)

type 'a t
  (** Type of signals returning values of type ['a] *)

val event : 'a t -> 'a React.event
  (** The event which occurs each time the signal is received. *)

val disconnect : 'a t -> unit
  (** [disconnect signal] stops receiving the given signal.

      Note that [disconnect] is automatically called when [event
      signal] is garbage collected *)

(** {6 Signal configuration} *)

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
        OBus_signal.set_auto_match_rule false;
        OBus_signal.set_filters filters;
        let x = React.E.map (...) signal#event
      ]}
      you can write:
      {[
        let x = React.E.map (...) (OBus_signal.init ~filters ~auto_match_rule:false  (Foo.bar proxy))
      ]}
  *)

(** {6 Signal creation} *)

(** These are primitives for creating signals. {!OBus_proxy} offers
    more convenient functions to define signls. *)

val connect :
  connection : OBus_connection.t ->
  ?sender : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, _) OBus_type.cl_sequence -> 'a t
  (** [connect ~connection ?sender ~path ~interface ~member typ]
      creates a signal which will receives event emited by the object
      with path [path] on peer with bus name [sender]. *)

val dyn_connect :
  connection : OBus_connection.t ->
  ?sender : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member -> unit -> OBus_value.sequence t
  (** Same as {!make} but returns dynamically typed values *)
