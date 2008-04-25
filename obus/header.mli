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

type flags = {
  (** Optionnal flags *)
  no_reply_expected : bool;
  no_auto_start : bool;
}

val default_flags : flags
  (** Defaults flags (all is false) *)

type fields = {
  (** Optionnal headers fields *)
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

type byte_order = Little_endian | Big_endian
    (** Message byte order *)

type t = {
  (** Header description *)
  byte_order : byte_order;
  message_type : message_type;
  flags : flags;
  serial : serial;
  fields : fields;

  (** Note: there is more fields in a real DBus messages but some like
      length or protocol version are added/checked automatically by
      OBus *)
}
