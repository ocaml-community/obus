(*
 * oBus_message.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Message description *)

type serial = int32

(** {6 Message structure} *)

type body = OBus_value.V.sequence
    (** The body is a sequence of dynamically typed values *)

type typ =
  | Method_call of OBus_path.t * OBus_name.interface * OBus_name.member
  | Method_return of serial
  | Error of serial * OBus_name.error
  | Signal of OBus_path.t * OBus_name.interface * OBus_name.member

(** flags *)
type flags = {
  no_reply_expected : bool;
  no_auto_start : bool;
}

val no_reply_expected : flags -> bool
  (** [no_reply_expected] projection *)

val no_auto_start : flags -> bool
  (** [no_auto_start] projection *)

val make_flags : ?no_reply_expected:bool -> ?no_auto_start:bool -> unit -> flags
  (** Creates message flags. All optional arguments default to
      [false] *)

val default_flags : flags
  (** All false *)

type t = {
  flags : flags;
  serial : serial;
  typ : typ;
  destination : OBus_name.bus;
  sender : OBus_name.bus;
  body : body;
}

(** {8 Projections} *)

val flags : t -> flags
val serial : t -> serial
val typ : t -> typ
val destination : t -> OBus_name.bus
val sender : t -> OBus_name.bus
val body : t -> body

(** {6 Helpers for creating messages} *)

(** Note that when creating a message the serial field is not
    relevant, it is overridden by {!OBus_connection} at
    sending-time *)

val make :
  ?flags : flags ->
  ?serial : serial ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  typ : typ ->
  body -> t

val method_call :
  ?flags : flags ->
  ?serial : serial ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  body -> t

val method_return :
  ?flags : flags ->
  ?serial : serial ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  reply_serial : serial ->
  body -> t

val error :
  ?flags : flags ->
  ?serial : serial ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  reply_serial : serial ->
  error_name : OBus_name.error ->
  body -> t

val signal :
  ?flags : flags ->
  ?serial : serial ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  body -> t

(** {6 Errors} *)

exception Invalid_reply of string
  (** Exception raised when the signature of the reply to a method
      call does not match the expected signature. The argument is an
      error message. *)

val invalid_reply : method_call : t -> expected_signature : OBus_value.signature -> method_return : t -> exn
  (** [invalid_reply ~method_call ~expected_signature ~method_return]
      @return an {!Invalid_reply} exception with a informative
      description of the error.
      @raise Invalid_argument if [method_call] is not a method
      call message or [method_return] is not a method return
      message *)

(** {6 Pretty-printing} *)

val print : Format.formatter -> t -> unit
  (** Print a message on a formatter *)
