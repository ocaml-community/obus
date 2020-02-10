(*
 * oBus_lowlevel.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Message serialization/deserialization *)

exception Data_error of string
  (** Exception raised when a message can not be sent. The parameter is an
      error message.

      Possible reasons are: the message is too big or contains arrays
      that are too big. *)

exception Protocol_error of string
  (** Exception raised when a received message is not valid.

      Possible reasons are:

      - a size limit is exceeded
      - a name/string/object-path is not valid
      - a boolean value is other than 0 or 1
      - ... *)

val read_message : Lwt_io.input_channel -> OBus_message.t Lwt.t
  (** [read_message ic] deserializes a message from a channel. It
      fails if the message contains file descriptors. *)

val write_message : Lwt_io.output_channel -> ?byte_order : Lwt_io.byte_order -> OBus_message.t -> unit Lwt.t
  (** [write_message oc ?byte_order message] serializes a message to a
      channel. It fails if the message contains file descriptors. *)

val message_of_string : string -> Unix.file_descr array -> OBus_message.t
  (** [message_of_string buf fds] returns a message from a
      string. [fds] is used to resolv file descriptors the message may
      contains. *)

val string_of_message : ?byte_order : Lwt_io.byte_order -> OBus_message.t -> string * Unix.file_descr array
  (** Marshal a message into a string. Returns also the list of file
      descriptors that must be sent with the message. *)

type reader
  (** A reader which support unix fd passing *)

val reader : Lwt_unix.file_descr -> reader
  (** [reader unix_socket] creates a reader from a unix socket *)

val read_message_with_fds : reader -> OBus_message.t Lwt.t
  (** Read a message with its file descriptors from the given
      reader *)

val close_reader : reader -> unit Lwt.t
  (** [close_reader reader] closes the given reader.

      Note: this does not close the underlying file descriptor. *)

type writer
  (** A writer which support unix fd passing *)

val writer : Lwt_unix.file_descr -> writer
  (** [writer unix_socket] creates a writer from a unix socket *)

val write_message_with_fds : writer -> ?byte_order : Lwt_io.byte_order -> OBus_message.t -> unit Lwt.t
  (** Write a message with its file descriptors on the given writer *)

val close_writer : writer -> unit Lwt.t
  (** [close_writer writer] closes the given writer.

      Note: this does not close the underlying file descriptor. *)

