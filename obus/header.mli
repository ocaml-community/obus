(*
 * header.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Header description. *)

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

type method_call_typ =
    [ `Method_call of path * interface option * member ]
type method_return_typ =
    [ `Method_return of reply_serial ]
type error_typ =
    [ `Error of reply_serial * error_name ]
type signal_typ =
    [ `Signal of path * interface * member ]

type any_typ =
    [ method_call_typ
    | method_return_typ
    | error_typ
    | signal_typ ]

(** flags *)
type flags = {
  no_reply_expected : bool;
  no_auto_start : bool;
}

type 'a header = {
  flags : flags;
  serial : serial;
  message_type : 'a;
  (** Optionnals header fields *)
  destination : destination option;
  sender : sender option;
  (** No signature is equivalent to "" *)
  signature : signature;
}

type t = any_typ header

type method_call = method_call_typ header
type method_return = method_return_typ header
type signal = signal_typ header
type error = error_typ header

val default_flags : flags
  (** All false *)

(** {6 Helpers for creation of headers} *)

(** Note that the function [Connection.send_*] will ignore the
    [signature] and [serial] fields *)

val method_call :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  ?signature:signature ->
  path:path ->
  ?interface:interface ->
  member:member -> unit -> [> method_call_typ ] header

val method_return :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  ?signature:signature ->
  reply_serial:serial -> unit -> [> method_return_typ ] header

val error :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  ?signature:signature ->
  reply_serial:serial ->
  error_name:error_name -> unit -> [> error_typ ] header

val signal :
  ?flags:flags ->
  ?sender:sender ->
  ?destination:destination ->
  ?signature:signature ->
  path:path ->
  interface:interface ->
  member:member -> unit -> [> signal_typ ] header
