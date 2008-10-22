(*
 * oBus_info.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Various informations *)

val version : string
  (** version of obus *)

val machine_uuid : OBus_uuid.t Lwt.t Lazy.t
  (** UUID of the machine we are running on *)

val protocol_version : int
  (** The version of the DBus protocol implemented by the library *)

val max_name_length : int
  (** Maximum length of a name (=255). This limit applies to bus
      names, interfaces, and members *)

val max_array_size : int
  (** Maximum size for the marshaled representation of an array. In
      this version of the protocol this is 2^26 bytes (64MB). *)

val max_message_size : int
  (** Maximum size of a message. In this version of the protocol this
      is 2^27 bytes (128MB). *)

type byte_order = Little_endian | Big_endian

val native_byte_order : byte_order
  (** Byte order of the current architecture. It is used as default
      for sending messages. *)

val verbose : bool ref
  (** [true] is the environment variable OBUS_LOG is set. This will
      make obus verbose. *)

val debug : bool ref
  (** [true] is the environment variable OBUS_LOG is set to
      "debug". This will make obus more verbose. *)

val dump : bool ref
  (** [true] is the environment variable OBUS_LOG is set to
      "dump". This will dump all incoming and outgoind message on
      stderr. *)
