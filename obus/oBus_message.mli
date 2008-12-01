(*
 * oBus_message.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Message description *)

type serial = int32

(** Message types with their recquired/optinnal parameters *)

type body = OBus_value.sequence
    (** The body is a sequence of dynamically typed values *)

type method_call_type =
    [ `Method_call of OBus_path.t * OBus_name.interface option * OBus_name.member ]

type method_return_type =
    [ `Method_return of serial
        (** Contains the serial for which this message is a reply *) ]

type error_type =
    [ `Error of serial * OBus_name.error ]

type signal_type =
    [ `Signal of OBus_path.t * OBus_name.interface * OBus_name.member ]

type any_type =
    [ method_call_type
    | method_return_type
    | error_type
    | signal_type ]

(** flags *)
type flags = {
  no_reply_expected : bool;
  no_auto_start : bool;
}

val no_reply_expected : flags -> bool
val no_auto_start : flags -> bool

val make_flags : ?no_reply_expected:bool -> ?no_auto_start:bool -> unit -> flags
  (** Optionnal arguments default to true *)

val default_flags : flags
  (** All false *)

type 'typ t = {
  flags : flags;
  serial : serial;
  typ : 'typ;
  destination : OBus_name.bus option;
  sender : OBus_name.bus option;
  body : body;
}

type method_call = method_call_type t
type method_return = method_return_type t
type signal = signal_type t
type error = error_type t
type any = any_type t

val body : 'a t -> body
val flags : 'a t -> flags
val serial : 'a t -> serial
val typ : 'a t -> 'a
val destination : 'a t -> OBus_name.bus option
val sender : 'a t -> OBus_name.bus option
val path : [< method_call_type | signal_type ] t -> OBus_path.t
val interface : [< method_call_type | signal_type ] t -> OBus_name.interface option
val signal_interface : signal -> OBus_name.interface
val member : [< method_call_type | signal_type ] t -> OBus_name.member
val reply_serial : [< method_return_type | error_type ] t -> serial
val error_name : error -> OBus_name.error

(** {6 Creation of header} *)

(** Note that when creating an header the serial field is not
    relevant, it is overridden at sending-time *)

val make :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.bus ->
  ?destination:OBus_name.bus ->
  typ:'a ->
  body -> 'a t

val method_call :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.bus ->
  ?destination:OBus_name.bus ->
  path:OBus_path.t ->
  ?interface:OBus_name.interface ->
  member:OBus_name.member ->
  body -> method_call

val method_return :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.bus ->
  ?destination:OBus_name.bus ->
  reply_serial:serial ->
  body -> method_return

val error :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.bus ->
  ?destination:OBus_name.bus ->
  reply_serial:serial ->
  error_name:OBus_name.error ->
  body -> error

val signal :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.bus ->
  ?destination:OBus_name.bus ->
  path:OBus_path.t ->
  interface:OBus_name.interface ->
  member:OBus_name.member ->
  body -> signal

(** {6 Pretty-printing} *)

val print : Format.formatter -> [< any_type ] t -> unit
  (** Print a message on a formatter *)
