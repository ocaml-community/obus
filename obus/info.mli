(*
 * info.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Various informations *)

val native_byte_order : Header.byte_order
  (** Byte order of the current architecture. It is used as default
      for sended message. *)

val protocol_version : int
  (** The version of the DBus protocol implemented by the library *)
