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

(** Type of each header field *)
type path = string
type interface = string
type member = string
type error_name = string
type reply_serial = serial
type destination = string
type sender = string
type signature = string

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

type ('typ, 'body) message = (*private*) {
  flags : flags;
  serial : serial;
  typ : 'typ;
  (** Optionnals header fields *)
  destination : destination option;
  sender : sender option;
  (** No signature is equivalent to "" *)
  signature : signature;
  body : 'body;
}

val flags : ('a, 'b) message -> flags
val serial : ('a, 'b) message -> serial
val typ : ('a, 'b) message -> 'a
val destination : ('a, 'b) message -> destination option
val sender : ('a, 'b) message -> sender option
val signature : ('a, 'b) message -> signature
val body : ('a, 'b) message -> 'b

type body = Values.values
    (** A message body is just a list of DBus values *)

type t = (any_type, body) message

type method_call = (method_call_type, body) message
type method_return = (method_return_type, body) message
type signal = (signal_type, body) message
type error = (error_type, body) message

(** {6 Creation of messages} *)

(** Note that when creating a message the serial field is not
    relevant *)

val make :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  typ:'a ->
  body:body -> unit -> ('a, body) message

val method_call :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  path:path ->
  ?interface:interface ->
  member:member ->
  body:body -> unit -> ([> method_call_type ], body) message

val method_return :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  reply_serial:serial ->
  body:body -> unit -> ([> method_return_type ], body) message

val error :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  reply_serial:serial ->
  error_name:error_name ->
  body:body -> unit -> ([> error_type ], body) message

val signal :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  path:path ->
  interface:interface ->
  member:member ->
  body:body -> unit -> ([> signal_type ], body) message

(**/**)

open Wire
type send_body = byte_order -> buffer -> ptr -> ptr
type recv_body = byte_order * buffer * ptr
type 'a intern_send = ('a, send_body) message
type 'a intern_recv = ('a, recv_body) message
val intern_make_send :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  signature:signature ->
  typ:'a ->
  body:send_body -> unit -> ('a, send_body) message
val intern_make_recv :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  serial:serial ->
  signature:signature ->
  typ:'a ->
  body:recv_body -> unit -> ('a, recv_body) message
val intern_user_to_send : ('a, body) message -> 'a intern_send
val intern_recv_to_user : 'a intern_recv -> ('a, body) message
