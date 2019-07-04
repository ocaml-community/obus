(*
 * oBus_connection.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus connections *)

(** This module implements manipulation of a D-Bus connection. A D-Bus
    connection is a channel opened with another application which also
    implement the D-Bus protocol. It is used to exchange D-Bus
    messages. *)

type t
    (** Type of D-Bus connections *)

val compare : t -> t -> int
  (** Same as [Pervasives.compare]. It allows this module to be used
      as argument to the functors [Set.Make] and [Map.Make]. *)

(** {6 Creation} *)

(** The following functions will return a connection which is ready to
    send and receive messages. You should use them only for direct
    connection to another application without passing through a
    message bus.

    Otherwise you should use [OBus_bus] or immediately call
    [OBus_bus.register_connection] after the creation. *)

val of_addresses : ?switch : Lwt_switch.t -> ?shared : bool -> OBus_address.t list -> t Lwt.t
  (** [of_addresses ?switch ?shared addresses] try to get a working
      D-Bus connection from a list of addresses. The server must be
      accessible from at least one of these addresses.

      If [shared] is true and a connection to the same server is
      already open, then it is used instead of [transport]. This is
      the default behaviour. *)

val loopback : unit -> t
  (** Create a connection with a loopback transport *)

val close : t -> unit Lwt.t
  (** Close a connection.

      All thread waiting for a reply will fail with the exception
      {!Connection_closed}.

      Notes:
      - when a connection is closed, the transport it use is
        closed too
      - if the connection is already closed, it does nothing
  *)

val active : t -> bool React.signal
  (** Returns whether a connection is active. *)

exception Connection_closed
  (** Raised when trying to use a closed connection *)

exception Connection_lost
  (** Raised when a connection has been lost *)

exception Transport_error of exn
  (** Raised when something wrong happens on the backend transport of
      the connection *)

(** {6 Informations} *)

val name : t -> OBus_name.bus
  (** Returns the unique name of the connection. This is only
      meaningful is the other endpoint of the connection is a
      message bus. If it is not the case it returns [""]. *)

(**/**)
val set_name : t -> OBus_name.bus -> unit
(**/**)

val transport : t -> OBus_transport.t
  (** [transport connection] get the transport associated with a
      connection *)

val can_send_basic_type : t -> OBus_value.T.basic -> bool
val can_send_single_type : t -> OBus_value.T.single -> bool
val can_send_sequence_type : t -> OBus_value.T.sequence -> bool
  (** [can_send_*_type connection typ] returns whether values of the
      given type can be sent through the given connection. *)

(** {6 Sending messages} *)

(** These functions are the low-level functions for sending
    messages. They take and return a complete message description *)

val send_message : t -> OBus_message.t -> unit Lwt.t
  (** [send_message connection message] send a message without
      expecting a reply. *)

val send_message_with_reply : t -> OBus_message.t -> OBus_message.t Lwt.t
  (** [send_message_with_reply connection message] Send a message and
      return a thread which waits for the reply (which is a method
      return or an error) *)

val send_message_keep_serial : t -> OBus_message.t -> unit Lwt.t
  (** Same as {!send_message} but does not generate a serial for the
      message.

      Warning: this is for implementing a D-Bus daemon only, not for
      casual use. *)

val send_message_keep_serial_with_reply : t -> OBus_message.t -> OBus_message.t Lwt.t
  (** Same as {!send_message_with_reply} but does not generate a serial
      for the message.

      Warning: this is for implementing a D-Bus daemon only, not for
      casual use. *)

(** {6 Helpers for calling methods} *)

val method_call :
  connection : t ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence ->
  o_args : 'b OBus_value.C.sequence ->
  'a -> 'b Lwt.t
  (** Calls a method using the given parameters, and waits for its
      reply. *)

val method_call_with_message :
  connection : t ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence ->
  o_args : 'b OBus_value.C.sequence ->
  'a -> (OBus_message.t * 'b) Lwt.t
  (** Same as {!method_call}, but also returns the reply message so
      you can extract informations from it. *)

val method_call_no_reply :
  connection : t ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence ->
  'a -> unit Lwt.t
  (** Same as {!method_call} but does not expect a reply *)

(** {6 General purpose filters} *)

(** Filters are functions that are applied on all incoming and
    outgoing messages.

    For incoming messages they are called before dispatching, for
    outgoing ones, they are called just before being sent.
*)

type filter = OBus_message.t -> OBus_message.t option
  (** The result of a filter must be:

      - [Some msg] where [msg] is the message given to the filter
      modified or not, which means that the message is replaced by
      this one

      - [None] which means that the message will be dropped, i.e. not
      dispatched or not sent *)

val incoming_filters : t -> filter Lwt_sequence.t
  (** Filters applied on incoming messages *)

val outgoing_filters : t -> filter Lwt_sequence.t
  (** Filters appllied on outgoing messages *)

(** {6 Connection local Storage} *)

(** Connection local storage allows to attach values to a
    connection. It is internally used by modules of obus. *)

type 'a key
  (** Type of keys. Keys are used to identify a resource attached to a
      connection. *)

val new_key : unit -> 'a key
  (** [new_key ()] generates a new key. *)

val get : t -> 'a key -> 'a option
  (** [get connection key] returns the data associated to [key] in
      connection, if any. *)

val set : t -> 'a key -> 'a option -> unit
  (** [set connection key value] attach [value] to [connection] under
      the key [key]. [set connection key None] will remove any
      occurence of [key] from [connection]. *)

(** {6 Errors handling} *)

(** Note: when a filter/signal handler/method_call handler raise an
    exception, it is just dropped. If {!OBus_info.debug} is set then a
    message is printed on [stderr] *)

val set_on_disconnect : t -> (exn -> unit Lwt.t) -> unit
  (** Sets the function called when a fatal error happen or when the
      conection is lost.

      Notes:
      - the default function does nothing
      - it is not called when the connection is closed using {!close}
      - if the connection is closed, it does nothing
  *)

(** {6 Low-level} *)

val of_transport : ?switch : Lwt_switch.t -> ?guid : OBus_address.guid -> ?up : bool -> OBus_transport.t -> t
  (** Create a D-Bus connection on the given transport. If [guid] is
      provided the connection will be shared.

      [up] tell whether the connection is initially up or down,
      default is [true]. *)

(** A connection can be up or down. Except for connections created with
    [of_transport], newly created connections are always up.

    When a connection is down, messages will not be dispatched *)

val state : t -> [ `Up | `Down ] React.signal
  (** Signal holding the current state of the connection *)

val set_up : t -> unit
  (** Sets up the connection if it is not already up *)

val set_down : t -> unit
  (** Sets down the connection if it is not already down *)
