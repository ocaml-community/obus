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

module type Wire = sig
  type 'a monad

  type input = {
    (** Needed functions for deserialization *)

    get_char : unit -> char monad;
    (** Read one character *)

    get_string : int -> string monad;
    (** [get_string len] read a string of length [len] *)
  }

  type output = {
    (** Needed functions for serialization *)

    put_char : char -> unit monad;
    (** Write one character *)

    put_string : string -> unit monad;
    (** Write a whole string *)
  }

  val get_message : input -> OBus_message.any monad
    (** [get_message input] deserialize one message from [input].

        @raise Protocol_error if the message is invalid. This is
        basically a fatal error and this means that the associated
        connection should be closed. *)

  val put_message : ?byte_order:OBus_info.byte_order -> OBus_message.any -> int * (output -> unit monad)
    (** [put_message ?byte_order message] create a message serializer.

        - if it fail, this means that the message can not be marshaled
        for some reason (for example in contains incorrect or too big
        data). This kind of error can be ignored. Only {!Data_error}
        may be raised.

        - if it succeed, it returns the size of the serialized message
        and a serializer. The serializer take an output and serialize
        the message on it. Errors raised at this stage are only io
        errors and are probably fatals (message partially sent). *)
end

module Make_wire(Monad : OBus_monad.S) : Wire with type 'a monad = 'a Monad.t
  (** Create wire functions with the given monad *)

module Lwt_wire : Wire with type 'a monad = 'a Lwt.t
  (** Lwt operations *)

val lwt_input_of_channel : Lwt_chan.in_channel -> Lwt_wire.input
val lwt_output_of_channel : Lwt_chan.out_channel -> Lwt_wire.output
  (** Create an input/output from a lwt in/out-channel *)

val get_message_size : string -> int -> int
  (** [get_message_size buf ofs] extract the size of a message from
      the start of its header. [buf] must contains at [ofs] the first
      16 bytes of the message. The size returned is the size of the
      whole message, including the 16 first bytes.

      @raise [Invalid_argument "OBus_lowlevel.get_message_size"] if
      one of [ofs+0], [ofs+1], ... [ofs+15] is not a valid index for
      [buf]

      @raise [Protocol_error msg] if the header is not valid, the
      protocol version is not supported or the message is too big *)

(** {6 Transport} *)

type transport = {
  recv : unit -> OBus_message.any Lwt.t;
  send : OBus_message.any -> unit Lwt.t;
  shutdown : unit -> unit;
}

val make_transport :
  recv:(unit -> OBus_message.any Lwt.t) ->
  send:(OBus_message.any -> unit Lwt.t) ->
  shutdown:(unit -> unit) -> transport

val recv : transport -> OBus_message.any Lwt.t
val send : transport -> OBus_message.any -> unit Lwt.t
val shutdown : transport -> unit

val loopback : unit -> transport
  (** Loopback transport, each message sent is received on the same
      transport *)

val transport_of_addresses : ?mechanisms:OBus_auth.client_mechanism list ->
  OBus_address.t list -> (OBus_address.guid * transport) Lwt.t
  (** Try to make a working transport from a list of addresses. Return
      also the guid of the server address *)
