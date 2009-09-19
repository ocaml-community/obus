(*
 * oBus_lowlevel.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Low-level control of OBus *)

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

(** {6 Message serialization/deserialization} *)

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

(** {6 Transports} *)

type transport = {
  recv : unit -> OBus_message.t Lwt.t;
  send : OBus_message.t -> unit Lwt.t;
  shutdown : unit -> unit Lwt.t;
}

(** Note for implementation of new transports: OBus send messages one
    by one, you can rely on that. *)

val make_transport :
  recv:(unit -> OBus_message.t Lwt.t) ->
  send:(OBus_message.t -> unit Lwt.t) ->
  shutdown:(unit -> unit Lwt.t) -> transport

val recv : transport -> OBus_message.t Lwt.t
val send : transport -> OBus_message.t -> unit Lwt.t
val shutdown : transport -> unit Lwt.t

val transport_of_channels : Lwt_io.input_channel * Lwt_io.output_channel -> transport
  (** [transport_of_channels (ic, oc)] creates a transport from a pair
      of channels. The [shutdown] function only flushes the output
      channel. *)

val loopback : unit -> transport
  (** Loopback transport, each message sent is received on the same
      transport *)

val transport_of_addresses :
  ?mechanisms : OBus_auth.client_mechanism list ->
  OBus_address.t list ->
  (OBus_address.guid * transport) Lwt.t
    (** Try to make a working transport from a list of
        addresses. Return also the guid of the server address *)
