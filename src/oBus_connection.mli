(*
 * oBus_connection.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Inerface to D-Bus connection *)

(** This module implement manipulation of a D-Bus connection. A D-Bus
    connection is a channel opened with another application which also
    implement the D-Bus protocol. It is used to exchange D-Bus
    messages. *)

type t = OBus_private.packed_connection
 with obus(sequence)

(** {6 Creation} *)

(** The following functions will return a connection which is ready to
    send and receive messages. You should use them only for direct
    connection to another application without passing through a
    message bus.

    Otherwise you should use [OBus_bus] or immediatly call
    [OBus_bus.register_connection] after the creation. *)

val of_addresses : ?shared:bool -> OBus_address.t list -> t Lwt.t
  (** [of_addresses shared addresses] try to get a working D-Bus
      connection from a list of addresses. The server must be
      accessible from at least one of these addresses.

      If [shared] is true and a connection to the same server is
      already open, then it is used instead of [transport]. This is
      the default behaviour. *)

val loopback : t
  (** Connection with a loopback transport *)

val close : t -> unit Lwt.t
  (** Close a connection.

      All thread waiting for a reply will fail with the exception
      {!Connection_closed}.

      Notes:
      - when a connection is closed, the transport it use is
      closed too
      - if the connection is already closed, it does nothing
  *)

val running : t -> bool
  (** Return weather a connection is running. *)

val watch : t -> unit Lwt.t
  (** Return a waiting thread which is wakeup when the connection is
      closed.

      If the connection is closed using {!close} then it return [()].

      If the connection is closed for an external reason it fail with
      the exception which make the connection to crash. *)

exception Connection_closed
  (** Raise when tring to use a normally closed connection *)

exception Connection_lost
  (** Raised when a connection has been lost *)

exception Transport_error of exn
  (** Raised when something wrong happen on the backend transport of
      the connection *)

(** {6 Informations} *)

val transport : t -> OBus_transport.t
  (** [transport connection] get the transport associated with a
      connection *)

val name : t -> OBus_name.bus option
  (** Unique name of the connection. This is only relevant if the
      other side of the connection is a message bus.

      In this case this is the unique name assigned by the message bus
      for the lifetime of the connection.

      In other cases it is [None]. *)

(** {6 Contextes} *)

exception Context of t * OBus_message.t
  (** The context used to cast messages *)

val obus_context : (t * OBus_message.t) OBus_type.sequence
  (** Returns the context of a message *)

(** {6 Sending messages} *)

(** These functions are the low-level functions for sending
    messages. They take and return a complete message description *)

val send_message : t -> OBus_message.t -> unit Lwt.t
  (** [send_message connection message] send a message without
      expecting a reply. *)

val send_message_with_reply : t -> OBus_message.t -> OBus_message.t Lwt.t
  (** [send_message_with_reply connection message] Send a message and
      return a thread which wait for the reply (which is a method
      return or an error) *)

(** {6 Helpers} *)

val method_call : t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, 'b Lwt.t, 'b) OBus_type.func -> 'a
  (** Send a method call and wait for the reply *)

val method_call_no_reply : t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, unit Lwt.t, unit) OBus_type.func -> 'a
  (** Send a method call without waiting for the reply. The
      [no_reply_expected] flag is automatically set to [true]. *)

val method_call' : t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body ->
  ('a, _) OBus_type.cl_sequence -> 'a Lwt.t
  (** Same thing but take the body of the message as a
      dynamically-typed value. *)

val dyn_method_call : t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body -> OBus_message.body Lwt.t
  (** Send a method call and wait for the reply. *)

val dyn_method_call_no_reply : t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  ?interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body -> unit Lwt.t

val emit_signal : t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, _) OBus_type.cl_sequence -> 'a -> unit Lwt.t
  (** Emit a signal *)

val dyn_emit_signal : t ->
  ?flags : OBus_message.flags ->
  ?sender : OBus_name.bus ->
  ?destination : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_message.body -> unit Lwt.t

val send_reply : t -> OBus_message.t -> ('a, _) OBus_type.cl_sequence -> 'a -> unit Lwt.t
  (** [send_reply connection method_call reply] Send a reply to a
      method call *)

val dyn_send_reply : t -> OBus_message.t -> OBus_value.sequence -> unit Lwt.t

val send_error : t -> OBus_message.t -> OBus_error.name -> OBus_error.message -> unit Lwt.t
  (** Send an error message in reply to a method call *)

val send_exn : t -> OBus_message.t -> exn -> unit Lwt.t
  (** [send_exn connection method_call exn] is a short-hand for
      passing [exn] through [OBus_error.unmake] then calling
      [send_error].

      It send the dbus error ["ocaml.Exception"] with the exception as
      message if it is not registred as a D-Bus exception. Note that
      this is bad thing since D-Bus errors are supposed to be user
      readable. *)

(** {6 Filters} *)

(** Filters are functions whose are applied on all incoming and
    outgoing messages.

    For incoming messages they are called before dispatching, for
    outgoing, they are called just before being sent.
*)

type filter = OBus_message.t -> OBus_message.t option
  (** The result of a filter must be:

      - [Some msg] where [msg] is the message given to the filter
      modified or not, which means that the message is replaced by
      this one

      - [None] which means that the message will be dropped, i.e. not
      dispatched or not sent *)

val incoming_filters : t -> filter Lwt_sequence.t
val outgoing_filters : t -> filter Lwt_sequence.t

(** {6 Errors handling} *)

(** Note: when a filter/signal handler/method_call handler raise an
    exception, it is just dropped. If {!OBus_info.debug} is set then a
    message is printed on [stderr] *)

val on_disconnect : t -> (exn -> unit) ref
  (** Function called when a fatal error happen or when the conection
      is lost.

      Notes:
      - the default function do nothing
      - it is not called when the connection is closed using
      {!close}
      - for connection to a message bus, the behaviour is different,
      see {!OBus_bus} for explanation *)

(** {6 Low-level} *)

val of_transport : ?guid : OBus_address.guid -> ?up : bool -> OBus_transport.t -> t
  (** Create a D-Bus connection on the given transport. If [guid] is
      provided the connection will be shared.

      [up] tell weather the connection is initially up or down,
      default is [true]. *)

(** A connection can be up or down, expect for connection created with
    [of_transport], newly created connection are always up.

    When a connection is down, messages will not be dispatched *)

val is_up : t -> bool
val set_up : t -> unit
val set_down : t -> unit
