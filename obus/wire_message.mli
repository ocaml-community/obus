(*
 * wireMessage.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Module used to receive or send an entire message *)

type recv = {
  recv_header : OBus_header.any;
  recv_signature : OBus_value.signature;
  recv_byte_order : OBus_info.byte_order;
  recv_body_start : int;
  recv_length : int;
  recv_buffer : string;
}

val recv_one_message : OBus_internals.connection -> string -> recv Lwt.t
  (** [recv_one_message connection buffer] read one a message from the given
      connection.

      It can raise one of these fatal errors: [Transport.Error],
      [Wire.Reading_error]. Any other errors are convertion errors. *)

type 'a send = {
  send_header : 'a OBus_header.t;
  send_signature : OBus_value.signature;
  send_writer : OBus_internals.writer;
  send_byte_order : OBus_info.byte_order;
  send_serial : int32;
}

val send_one_message : OBus_internals.connection -> 'a send -> string -> (string * exn option) Lwt.t
  (** [send_one_message connection send buffer] write and send one
      message on the given connection. It return the same buffer as
      given in argument or a new buffer if a growing was needed.

      It also return exception which may have been raised during the
      marshaling/sending of the message. It can be [Transport.Error]
      or [Wire.Writing_error]. Others errors are convertion errors. *)
