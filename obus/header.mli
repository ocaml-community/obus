(*
 * header.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Header description *)

type message_type =
  | Invalid
  | Method_call
  | Method_return
  | Error
  | Signal

type serial = int32
  (** Message identifier *)

(** Optionnal flags *)
type flags = {
  no_reply_expected : bool;
  no_auto_start : bool;
}

val default_flags : flags
  (** Defaults flags (all false) *)

(** Optionnal headers fields *)
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

val empty_fields : fields
  (** Fields where each entry is [None] *)

(** Message byte order *)
type byte_order = Little_endian | Big_endian

(** Header description *)
type ('length, 'serial)  t = {
  byte_order : byte_order;
  message_type : message_type;
  flags : flags;
  length : 'length;
  serial : 'serial;
  fields : fields;
}
    (** Note: The protocol version is not represented here because it
        is added/checked automatically by OBus. You can not
        communicate with an application that does not have the same
        protocol version as you. *)

type send = (unit, unit) t
type recv = (int, serial) t
    (** An header can be a 'send' header, which means that the message
        is destined to be send over the bus, so we must not fill
        [serial] and [length] fields because it will be done
        automatically by the library. Or a 'recv' header, which mean
        that the message has been received, so we can look at these
        informations. *)

(**/**)

open Wire

module type Reader = sig
  val read_constant_part : buffer -> message_type * flags * int * serial * int
  val read_fields : buffer -> int -> fields
end

module type Writer = sig
  val write_constant_part : buffer -> message_type -> flags -> int -> serial -> unit
  val write_fields : buffer -> fields -> buffer * int
end

module LEReader : Reader
module BEReader : Reader
module LEWriter : Writer
module BEWriter : Writer
