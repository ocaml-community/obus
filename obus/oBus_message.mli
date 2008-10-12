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
    [ `Method_call of OBus_path.t * OBus_name.Interface.t option * OBus_name.Member.t ]

type method_return_type =
    [ `Method_return of serial
        (** Contains the serial for which this message is a reply *) ]

type error_type =
    [ `Error of serial * OBus_name.Error.t ]

type signal_type =
    [ `Signal of OBus_path.t * OBus_name.Interface.t * OBus_name.Member.t ]

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
  destination : OBus_name.Connection.t option;
  sender : OBus_name.Connection.t option;
  body : body;
}
constraint 'typ = [< any_type ]

type method_call = method_call_type t
type method_return = method_return_type t
type signal = signal_type t
type error = error_type t
type any = any_type t

val body : 'a t -> body
val flags : 'a t -> flags
val serial : 'a t -> serial
val typ : 'a t -> 'a
val destination : 'a t -> OBus_name.Connection.t option
val sender : 'a t -> OBus_name.Connection.t option
val path : [< method_call_type | signal_type ] t -> OBus_path.t
val interface : [< method_call_type | signal_type ] t -> OBus_name.Interface.t option
val signal_interface : signal -> OBus_name.Interface.t
val member : [< method_call_type | signal_type ] t -> OBus_name.Member.t
val reply_serial : [< method_return_type | error_type ] t -> serial
val error_name : error -> OBus_name.Error.t

(** {6 Creation of header} *)

(** Note that when creating an header the serial field is not
    relevant, it is overridden at sending-time *)

val make :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  typ:'a ->
  body -> 'a t

val method_call :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  path:OBus_path.t ->
  ?interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  body -> method_call

val method_return :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  reply_serial:serial ->
  body -> method_return

val error :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  reply_serial:serial ->
  error_name:OBus_name.Error.t ->
  body -> error

val signal :
  ?flags:flags ->
  ?serial:serial ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  path:OBus_path.t ->
  interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  body -> signal

(** {6 Pretty-printing} *)

val print : Format.formatter -> 'a t -> unit
  (** Print a message on a formatter *)
