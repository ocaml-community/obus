(*
 * message.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus message description *)

type typ =
  | Invalid
  | Method_call
  | Method_return
  | Error
  | Signal

type serial

type flag =
  | No_reply_expected
  | No_auto_start

type field =
  | Path of string
  | Member of string
  | Interface of string
  | Error_name of string
  | Reply_serial of serial
  | Destination of string
  | Sender of string
  | Signature of string

type header = typ * flag list * field list
type body = Value.t

type t = header * body
