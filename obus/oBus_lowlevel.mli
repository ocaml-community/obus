(*
 * oBus_lowlevel.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Low-level control of OBus *)

(** {6 Message serialization/deserialization} *)

exception Protocol_error of string

val get_message : Lwt_chan.in_channel -> OBus_message.any Lwt.t
  (** Receive one message.

      @raise Protocol_error if the message is invalid. This is
      basically a fatal error and this means that the associated
      connection should be closed. *)

val put_message : ?byte_order:OBus_info.byte_order -> OBus_message.any -> (Lwt_chan.out_channel -> unit Lwt.t) Lwt.t
  (** Send one message.

      Sending a message is done is two stage, first we create a
      marshaler from a message and a byte order,

      - if it fail this means that the message can not be marshaled
      for some reason (for example in contains incorrect or too big
      data). This kind of error can be ignored. Only [Failure] may be
      raised here.

      - if it succeed it return a closure which when applied on an
      output channel marshal the message on it and flush the channel.
      Errors raised at this stage are only io errors and are probably
      fatals (message partially sent). *)

(** {6 Transport} *)

type transport = {
  recv : unit -> OBus_message.any Lwt.t;
  send : OBus_message.any -> (unit -> unit Lwt.t) Lwt.t;
  shutdown : unit -> unit;
}

val make_transport :
  recv:(unit -> OBus_message.any Lwt.t) ->
  send:(OBus_message.any -> (unit -> unit Lwt.t) Lwt.t) ->
  shutdown:(unit -> unit) -> transport

val recv : transport -> OBus_message.any Lwt.t
val send : transport -> OBus_message.any -> (unit -> unit Lwt.t) Lwt.t
val shutdown : transport -> unit

val loopback : unit -> transport
  (** Loopback transport, each message sent is received on the same
      transport *)

val transport_of_addresses : ?mechanisms:OBus_auth.client_mechanism list ->
  OBus_address.t list -> (OBus_address.guid * transport) Lwt.t
  (** Try to make a working transport from a list of addresses. Return
      also the guid of the server address *)
