(*
 * header.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Message description. *)

type serial = int32

(** Type of each message part *)
type path = Path.t
type interface = string
type member = string
type error_name = string
type reply_serial = serial
type destination = string
type sender = string
type body = Values.values

type method_call_type =
    [ `Method_call of path * interface option * member ]
type method_return_type =
    [ `Method_return of reply_serial ]
type error_type =
    [ `Error of reply_serial * error_name ]
type signal_type =
    [ `Signal of path * interface * member ]

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

val default_flags : flags
  (** All false *)

type ('typ, 'body) _message = {
  flags : flags;
  serial : serial;
  typ : 'typ;
  destination : destination option;
  sender : sender option;
  body : 'body;
}

type 'a message = ('a, Values.values) _message

val flags : ('a, 'b) _message -> flags
val serial : ('a, 'b) _message -> serial
val typ : ('a, 'b) _message -> 'a
val destination : ('a, 'b) _message -> destination option
val sender : ('a, 'b) _message -> sender option
val body : ('a, 'b) _message -> 'b

val signature : 'a message -> string
  (** shorthand for [(Values.signature_of_dtypes
      (Values.dtypes_of_values (body message)))] *)

type t = any_type message

type method_call = method_call_type message
type method_return = method_return_type message
type signal = signal_type message
type error = error_type message

(** {6 Creation of messages} *)

(** Note that when creating a message the serial field is not
    relevant *)

val make :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  typ:'a ->
  body:body -> unit -> 'a message

val method_call :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  path:path ->
  ?interface:interface ->
  member:member ->
  body:body -> unit -> [> method_call_type ] message

val method_return :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  reply_serial:serial ->
  body:body -> unit -> [> method_return_type ] message

val error :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  reply_serial:serial ->
  error_name:error_name ->
  body:body -> unit -> [> error_type ] message

val signal :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  path:path ->
  interface:interface ->
  member:member ->
  body:body -> unit -> [> signal_type ] message
