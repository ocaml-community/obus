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

(** Needed monadic operation *)
module type Monad = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
end

(** Needed functions for deserialization *)
module type Reader = sig
  include Monad

  val get_char : char t
  val get_string : int -> string t
end

(** Needed functions for serialization *)
module type Writer = sig
  include Monad

  val put_char : char -> unit t
  val put_string : string -> unit t
end

module Make_reader(Reader : Reader) : sig
  val get_message : OBus_message.t Reader.t
    (** [get_message] read one message.

        It fail with {!Protocol_error} if the message is invalid. *)
end

module Make_writer(Writer : Writer) : sig
  val put_message : ?byte_order:byte_order -> OBus_message.t -> int * unit Writer.t
    (** [put_message ?byte_order message] returns [(size, writer)]
        where [size] is the size of the marshaled message and [writer]
        is the message writer.

        It raises {!Data_error} if the message can not be marshaled
        for some reason.

        [writer] will not fail, unless [Writer.put_char] or
        [Writer.put_string] does. Moreover it is guaranted to write
        exactly [size] bytes. *)
end

val get_message : Lwt_chan.in_channel -> OBus_message.t Lwt.t
  (** Deserialize a message from a lwt channel *)

val put_message : ?byte_order:byte_order -> Lwt_chan.out_channel -> OBus_message.t -> unit Lwt.t
  (** Serialize a message to a lwt channel *)

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

val socket : Lwt_unix.file_descr -> Lwt_chan.in_channel * Lwt_chan.out_channel -> transport
  (** [socket fd (ic, oc)] creates a 'socket' transport. The file
      descriptor [fd] is only used for [shutdown].

      Note: [socket] optimize writing in the sense that it try to
      minimize the number of flushing operation, and so the number of
      syscalls. *)

val loopback : unit -> transport
  (** Loopback transport, each message sent is received on the same
      transport *)

val transport_of_addresses : ?mechanisms:OBus_auth.client_mechanism list ->
  OBus_address.t list -> (OBus_address.guid * transport) Lwt.t
  (** Try to make a working transport from a list of addresses. Return
      also the guid of the server address *)
