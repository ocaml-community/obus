(*
 * header.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type typ =
  | Invalid
  | Method_call
  | Method_return
  | Error
  | Signal

type serial = int32

type flags =
  | ReplyExpected
  | AutoStart

type fields =
  | Path of string
  | Interface of string
  | Member of string
  | Error_name of string
  | Reply_serial of serial
  | Destination of string
  | Sender of string
  | Signature of string

type byte_order = LittleEndian | BigEndian

type t = {
  byte_order : byte_order;
  typ : typ;
  flags : flags list;
  protocol_version : int;
  length : int;
  serial : serial;
  fields : fields list;
}
