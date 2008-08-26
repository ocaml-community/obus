(*
 * oBus_connection.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Inerface to DBus connection *)

(** This module implement low-level manipulation of a DBus connection.
    A DBus connection is a channel opened with another application
    which also implement the DBus protocol. It is used to exchange
    DBus messages.

    It is low-level because functions of this module deals directly
    with DBus messages, as header + body. *)

type t = OBus_internals.connection

(** {6 Creation} *)

(** The following functions will return a connection which is ready to
    send and receive messages. You should use them only for direct
    connection to another application without passing through a
    message bus.

    Otherwise you should use [OBus_bus] or immediatly call
    [OBus_bus.register_connection] after the creation. *)

val of_transport : ?shared:bool -> OBus_transport.t -> t Lwt.t
  (** [of_transport shared transport] create a dbus connection over
      the given transport. If [shared] is true and a connection to the
      same server is already open, then it is used instead of
      [transport], this is the default behaviour. *)

val of_authenticated_transport : ?shared:bool -> OBus_transport.t -> OBus_address.guid -> t
  (** Same as of_transport but assume that the authentification is
      done. Use it only if you know what you are doing. *)

val of_addresses : ?shared:bool -> OBus_address.t list -> t Lwt.t
  (** [of_addresses shared addresses] shorthand for obtaining
      transport and doing [of_transport] *)

val close : t -> unit
  (** Close a connection.

      All thread waiting for a reply will fail with the exception
      [Connection_closed].

      Note: when a connection is closed, the transport it use is
      closed too. *)

exception Connection_closed

(** {6 Informations} *)

val transport : t -> OBus_transport.t
  (** [transport connection] get the transport associated with a
      connection *)

val guid : t -> OBus_address.guid
  (** [guid connection] return the unique identifier of the server
      address *)

val name : t -> OBus_name.Connection_unique.t option
  (** Unique name of the connection. This is only relevant if the
      other side of the connection is a message bus.

      In this case this is the unique name assigned by the message bus
      for the lifetime of the connection.

      In other cases it is [None]. *)

(** {6 Sending messages} *)

(** These functions are the low-level functions for sending
    messages. They take and return a complete message description *)

val send_message : t -> 'a OBus_message.t -> unit Lwt.t
  (** [send_message connection message] send a message without
      expecting a reply *)

val send_message_with_reply : t -> OBus_message.method_call -> OBus_message.method_return Lwt.t
  (** [send_message_with_reply connection message] Send a message and
      return a thread which wait for the reply *)

(** {6 Helpers} *)

val method_call : t ->
  ?flags:OBus_message.flags ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  path:OBus_path.t ->
  ?interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> 'a
  (** Send a method call and wait for the reply *)

val kmethod_call : ((t -> 'a Lwt.t) -> 'b) ->
  ?flags:OBus_message.flags ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  path:OBus_path.t ->
  ?interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  ('c, 'b, 'a) OBus_type.ty_function -> 'c
  (** Same thing but take a continuation *)

val dmethod_call : t ->
  ?flags:OBus_message.flags ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  path:OBus_path.t ->
  ?interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  OBus_message.body -> OBus_message.body Lwt.t
  (** Dynamically-typed version, take the message body as a
      dynamically-typed value *)

val emit_signal : t ->
  ?flags:OBus_message.flags ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  path:OBus_path.t ->
  interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  ('a, unit Lwt.t, unit) OBus_type.ty_function -> 'a
  (** Emit a signal *)

val demit_signal : t ->
  ?flags:OBus_message.flags ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection.t ->
  path:OBus_path.t ->
  interface:OBus_name.Interface.t ->
  member:OBus_name.Member.t ->
  OBus_message.body -> unit Lwt.t

val send_error : t -> OBus_message.method_call -> OBus_error.name -> OBus_error.message -> unit Lwt.t
  (** Send an error message in reply to a method call *)

val send_exn : t -> OBus_message.method_call -> exn -> unit Lwt.t
  (** [send_exn connection method_call exn] is a short-hand for
      passing [exn] through [OBus_error.unmake] then calling
      [send_error]. It raise an [Invalid_argument] if the exception is
      not registred as a DBus exception. *)

val call_and_cast_reply : ('a, 'b, 'c) OBus_type.ty_function ->
  (OBus_message.body -> (t -> OBus_message.method_call -> 'c Lwt.t) -> 'b) -> 'a
  (** [call_and_cast_reply typ cont ...] Construct a message from
      using the given functionnal type, then pass it to [cont]. The
      second argument for [cont] is a function which will call the
      method, cast its reply and raise the standart error message if
      the cast fail *)

(** {6 Receiving signals} *)

type signal_receiver

val add_signal_receiver : t ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection_unique.t ->
  ?path:OBus_path.t ->
  ?interface:OBus_name.Interface.t ->
  ?member:OBus_name.Member.t ->
  ?args:(int * string) list ->
  ('a, unit, unit) OBus_type.ty_function -> 'a -> signal_receiver
  (** Add a signal receiver.

      Note that with a message bus, you probably need to also add a
      matching rule. *)

val dadd_signal_receiver : t ->
  ?sender:OBus_name.Connection.t ->
  ?destination:OBus_name.Connection_unique.t ->
  ?path:OBus_path.t ->
  ?interface:OBus_name.Interface.t ->
  ?member:OBus_name.Member.t ->
  ?args:(int * string) list ->
  (OBus_message.body -> unit) -> signal_receiver
  (** Dynamically-typed version. This one is more generic than
      [add_signal_handler] since it does not put constraint on the
      signal signature. *)

val enable_signal_receiver : signal_receiver -> unit
val disable_signal_receiver : signal_receiver -> unit
val signal_receiver_enabled : signal_receiver -> bool
  (** Manipulation of registred receiver *)

(** {6 Filters} *)

(** Filters are functions whose are applied on all incomming
    messages.

    Filters can be used to for debugging purpose or to write low-level
    DBus application (look at [samples/monitor.ml] to see an example
    of use of filters). *)

type filter_id

val add_filter : t -> (OBus_message.any -> unit) -> filter_id
  (** [add_filter connection filter] add a filter to the given
      connection. *)

val enable_filter : filter_id -> unit
val disable_filter : filter_id -> unit
val filter_enabled : filter_id -> bool
  (** Manipulation of registred filters *)

(** {6 Errors handling} *)

exception Protocol_error of string
  (** This exception is raised when an invalid DBus message is
      received. *)

(** Note: protocol and transport errors are considered as fatal
    errors. When a fatal error happen the connection is immediately
    closed. *)

exception Invalid_data of string
  (** Raised when a message can not be send because it contains
      invalid data *)

val on_disconnect : t -> (exn -> unit) ref
  (** Function called when a fatal error happen. The default behaviour
      is to print an error message and to exit the program. *)
