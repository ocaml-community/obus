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

exception Transport_error of exn

(** A transport is something which know how to receive and send
    messages. *)
class type transport = object
  method get_message : OBus_message.any Lwt.t
    (** Receive one message from the transport *)

  method put_message : OBus_message.any -> (unit -> unit Lwt.t) Lwt.t
    (** Send one message. It must act as [put_message] with output
        channels *)

  (** Notes:

      - [get_message] and [put_message] are supposed to return
      [End_of_file] when the transport is closed unexpectedly

      - all [get_message] calls are serialized as well as
      [put_message] ones, so there is no need for locks
  *)

  method shutdown : unit
    (** Shutdown the transport *)
end

(** A transport from the client point of view. In addition it must
    know how to handle authentication with the server. *)
class type client_transport = object
  inherit transport

  method client_authenticate : OBus_address.guid Lwt.t Lazy.t
    (** When forced, it should launch authentification on the
        transport and return the guid of the server address. *)
end

(** A transport from the server point of view, it must know how to
    authenticate the client. *)
class type server_transport = object
  inherit transport

  method server_authenticate : unit Lwt.t Lazy.t
    (** When forced, it should authenticate the client *)
end

(** Transport from a connected socket *)
class socket : Lwt_unix.file_descr -> object
  inherit transport
  val ic : Lwt_chan.in_channel
  val oc : Lwt_chan.out_channel
end

class client_socket : Lwt_unix.file_descr -> object
  inherit socket
  inherit client_transport
end

class server_socket :
  ?mechanisms:OBus_auth.server_mechanism list ->
  OBus_address.guid ->
  Lwt_unix.file_descr -> object
  inherit socket
  inherit server_transport
end

class loopback : transport
  (** Loopback transport, each message sent is received on the same
      transport *)

val client_transport_of_addresses : OBus_address.t list -> client_transport Lwt.t
  (** Try to make a working client transport from a list of
      addresses. This only works for transport which OBus internally
      handles *)
