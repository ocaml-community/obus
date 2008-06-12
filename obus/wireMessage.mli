(*
 * wireMessage.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Module used to receive or send an entire message *)

open Wire
open Message

type 'a send_message = ('a, string * (byte_order -> buffer -> ptr -> ptr)) _message
type 'a recv_message = ('a, string * (byte_order * buffer * ptr)) _message
    (** A "raw" message description. The body is seen as a signature
        (it must be given here) plus either a function to write
        effectively the body or the buffer containing it *)

type send = any_type send_message
type recv = any_type recv_message

type method_call_recv = method_call_type recv_message
type method_return_recv = method_return_type recv_message
type signal_recv = signal_type recv_message
type error_recv = error_type recv_message

type method_call_send = method_call_type send_message
type method_return_send = method_return_type send_message
type signal_send = signal_type send_message
type error_send = error_type send_message

val signature : ('a, string * 'b) _message -> string

val recv_one_message : Transport.t -> buffer -> recv * buffer
  (** [recv_one_message transport buffer] read one a message from the
      given transport. If buffer is not enough large then a new buffer
      is created. It return the parsed header of the message, the
      buffer acutally containing the body and the offset of the body
      start (aligned on an 8 boundary).

      It can raise one of these fatal errors: [Transport.Error],
      [Wire.Reading_error]. Any other errors are convertion errors. *)

val send_one_message : Transport.t -> buffer -> send -> buffer
  (** [send_one_message transport buffer message] write
      and send one message on the given transport.

      can raise one of [Transport.Error], [Wire.Writing_error]. Others
      errors are convertion errors. *)
