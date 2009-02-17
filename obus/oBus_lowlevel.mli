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

(** {Predefined serializer/deserializer} *)

val lwt_chan_get_message : Lwt_chan.in_channel -> OBus_message.t Lwt.t
  (** Deserialize a message from a lwt input channel *)

val lwt_chan_put_message : ?byte_order:byte_order -> Lwt_chan.out_channel -> OBus_message.t -> unit Lwt.t
  (** Serialize a message to a lwt output channel *)

val string_get_message : string -> OBus_message.t
  (** Deserialize a message from a string *)

val string_put_message : ?byte_order:byte_order -> OBus_message.t -> string
  (** Serialize a message into a string *)

type ochan
  (** Bufferized output, a bit more efficient than Lwt channel for use
      with obus. In particular there is a machinery to minimize the
      number of flush needed, which means that obus is able to send
      multiple messages at the same time, and so this means less
      system calls and context switchs. *)

type ichan
  (** Bufferized input, same thing *)

val get_default_buffer_size : int
  (** Default size of buffers for outputs and inputs. You can try to
      tweak that for performance. A bigger buffer means that more
      message can be sent/received simultaneously. At startup this
      variables is initialized from the environment variable
      "OBUS_BUFFER_SIZE" if available, if not the internal default is
      16Ko. *)

val set_default_buffer_size : int -> unit
  (** Change the default buffer size. Raises [Invalid_argument] if the
      given size is not valid *)

val make_ochan : ?buffer_size:int -> (string -> int -> int -> int Lwt.t) -> ochan
  (** Creates an output from the given write function. *)

val make_ichan : ?buffer_size:int -> (string -> int -> int -> int Lwt.t) -> ichan
  (** Creates an input from the given read function. *)

val ochan_flush : ochan -> unit Lwt.t
  (** Flush an output now. You should only call that before closing
      the backend transport to ensure everything has been sent. *)

val chan_get_message : ichan -> OBus_message.t Lwt.t
  (** Deserialize a message from an input *)

val chan_put_message : ?byte_order:byte_order -> ochan -> OBus_message.t -> unit Lwt.t
  (** Serialize a message into an input *)

val auth_stream_of_channels : ichan * ochan -> OBus_auth.stream
  (** Create a stream for authentication from a pair of channels *)

(** Note: for both input and output channel you can relly on the fact
    that calls to the read/write function are always serialized. *)

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

val transport_of_channels : ichan * ochan -> transport
  (** [transport_of_channels (ic, oc)] creates a transport from a pair
      of channels. Note: you probably want to adapt the [shutdown]
      function since it only flushes the output channel. *)

val loopback : unit -> transport
  (** Loopback transport, each message sent is received on the same
      transport *)

val transport_of_addresses : ?mechanisms:OBus_auth.client_mechanism list ->
  OBus_address.t list -> (OBus_address.guid * transport) Lwt.t
  (** Try to make a working transport from a list of addresses. Return
      also the guid of the server address *)
