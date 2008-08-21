(*
 * wireMessage.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Module used to receive or send an entire message *)

val recv_one_message : OBus_internals.connection -> OBus_message.any Lwt.t
  (** [recv_one_message connection] read one a message from the given
      connection.

      It can raise one of these fatal errors: [Transport.Error],
      [Wire.Reading_error]. Any other errors are convertion errors. *)

val send_one_message : OBus_internals.connection -> 'a OBus_message.t -> unit Lwt.t
  (** [send_one_message connection send buffer] write and send one
      message on the given connection.

      It can raise [Transport.Error] or [Wire.Writing_error]. Others
      errors are convertion errors. *)
