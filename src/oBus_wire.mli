(*
 * oBus_lowlevel.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Message serialization/deserialization *)

exception Data_error of string
  (** Exception raised a message can not be sent. The parameter is an
      error message.

      Possible reasons are: the message is too big or contains too big
      arrays. *)

exception Protocol_error of string
  (** Exception raised when a received message is not valid.

      Possible reasons are:

      - a size limit is exceeded
      - a name/string/object-path is not valid
      - a boolean value is other than 0 or 1
      - ... *)

type byte_order = Little_endian | Big_endian

val native_byte_order : byte_order
  (** Byte order of the current architecture. It is used as default
      for sending messages. *)

val read_message : Lwt_io.input_channel -> OBus_message.t Lwt.t
  (** [read_message ic] deserializes a message from a channel *)

val write_message : Lwt_io.output_channel -> ?byte_order : byte_order -> OBus_message.t -> unit Lwt.t
  (** [write_message oc ?byte_order message] serializes a message to a channel *)

val message_of_string : string -> OBus_message.t
  (** Return a message from a string *)

val string_of_message : ?byte_order : byte_order -> OBus_message.t -> string
  (** Marshal a message into a string *)
