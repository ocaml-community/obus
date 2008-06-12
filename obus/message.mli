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
type path = string
type interface = string
type member = string
type error_name = string
type reply_serial = serial
type destination = string
type sender = string
type signature = Values.dtypes
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

type ('typ, 'signature, 'body) _message = (*private*) {
  flags : flags;
  serial : serial;
  typ : 'typ;
  (** Optionnals header fields *)
  destination : destination option;
  sender : sender option;
  (** No signature is equivalent to "" *)
  signature : 'signature;
  body : 'body;
}

type 'a message = ('a, Values.dtypes, Values.values) _message

val flags : ('a, 'b, 'c) _message -> flags
val serial : ('a, 'b, 'c) _message -> serial
val typ : ('a, 'b, 'c) _message -> 'a
val destination : ('a, 'b, 'c) _message -> destination option
val sender : ('a, 'b, 'c) _message -> sender option
val signature : ('a, 'b, 'c) _message -> 'b
val body : ('a, 'b, 'c) _message -> 'c

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

(**/**)

open Wire
type 'a intern_send = ('a, string, byte_order -> buffer -> ptr -> ptr) _message
type 'a intern_recv = ('a, string, byte_order * buffer * ptr) _message
val intern_make_send :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  signature:string ->
  typ:'a ->
  body:(byte_order -> buffer -> ptr -> ptr) -> unit -> 'a intern_send
val intern_make_recv :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  serial:serial ->
  signature:string ->
  typ:'a ->
  body:(byte_order * buffer * ptr) -> unit -> 'a intern_recv
val intern_user_to_send : 'a message -> 'a intern_send
val intern_recv_to_user : 'a intern_recv -> 'a message
