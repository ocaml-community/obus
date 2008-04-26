(*
 * header.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type message_type =
  | Invalid
  | Method_call
  | Method_return
  | Error
  | Signal

type serial = int32
type flags = {
  no_reply_expected : bool;
  no_auto_start : bool;
}

let  default_flags = {
  no_reply_expected = false;
  no_auto_start = false;
}

type fields = {
  path : string option;
  member : string option;
  interface : string option;
  error_name : string option;
  reply_serial : serial option;
  destination : string option;
  sender : string option;
  signature : string option;
}

let empty_fields = {
  path = None;
  member = None;
  interface = None;
  error_name = None;
  reply_serial = None;
  destination = None;
  sender = None;
  signature = None;
}

type byte_order = Little_endian | Big_endian
type ('a, 'b) t = {
  byte_order : byte_order;
  message_type : message_type;
  flags : flags;
  length : 'a;
  serial : 'b;
  fields : fields;
}
type send = (unit, unit) t
type recv = (int, serial) t
