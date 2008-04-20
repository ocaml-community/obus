(*
 * header.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module L : LowLevel.S

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

type t = {
  byte_order : L.byte_order;
  typ : typ;
  flags : flags list;
  protocol_version : int;
  length : int;
  serial : serial;
  fields : fields list;
}

val write : L.buffer -> t -> L.ptr -> (int -> unit) * L.ptr
  (** [write buf header ptr] marshal an header, return a function that
      can be used for writing message size *)

val read : L.buffer -> t L.reader
  (** [read buf] load an header from its marshaled representation *)
