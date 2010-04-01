(*
 * oBus_method.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus Method calls *)

(** This module offers high-level function for calling methods and/or
    sending replies.

    Note that each function has two version:

    - a first one which takes an obus-type (see {!OBus_type})
      and any parameters it requires
    - a second, prefixed with [dyn_], meaning that the function only
      use dynamically typed values (see {!OBus_value})
*)

(** {6 Calling methods on a remote application} *)

val call :
  connection : OBus_connection.t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, 'b Lwt.t, 'b) OBus_type.func -> 'a
  (** Sends a method call and wait for the reply *)

val call_no_reply :
  connection : OBus_connection.t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, unit Lwt.t, unit) OBus_type.func -> 'a
  (** Send a method call without waiting for the reply. *)

(** {6 Calling methods with dynamically typed values} *)

val dyn_call :
  connection : OBus_connection.t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body -> OBus_message.body Lwt.t
  (** Dynamically-typed version of {!call} *)

val dyn_call_no_reply :
  connection : OBus_connection.t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body -> unit Lwt.t
  (** Dynamically-typed version of {!call_no_reply} *)

(** {6 Sending replies} *)

val return : context : OBus_type.context -> ('a, _) OBus_type.cl_sequence -> 'a -> unit Lwt.t
  (** [return ~context typ ...] sends a reply using the given
      context *)

val dyn_return : context : OBus_type.context -> OBus_value.sequence -> unit Lwt.t
  (** Dynamically-typed version of {!return} *)

val error : context : OBus_type.context -> name : OBus_error.name -> message : OBus_error.message -> unit Lwt.t
  (** [error ~context ~name ~message] sends an error as a reply *)

val fail : context : OBus_type.context -> exn -> unit Lwt.t
  (** [fail ~context exn] is a short-hand for passing [exn] through
      [OBus_error.cast] then sending the result with {!error}.

      It send the dbus error ["ocaml.Exception"] with the exception as
      message if it is not registred as a D-Bus exception. Note that
      this is bad thing since D-Bus errors are supposed to be user
      readable. *)
