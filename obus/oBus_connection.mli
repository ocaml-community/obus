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

type name = string
    (** A unique connection name, of the form ":X.XX" *)

val name : t -> name option
  (** Unique name of the connection. This is only relevant if the other
      side of the connection is a message bus.

      In this case this is the unique name assigned by the message bus
      for the lifetime of the connection.

      In other cases it is [None]. *)

(** {6 Sending messages} *)

val send_message : t -> 'a OBus_header.t -> ('b, unit Lwt.t, unit) OBus_type.ty_function -> 'b
  (** [send_message connection header typ ...] send a message without
      expecting a reply *)

val send_message_with_reply : t -> OBus_header.method_call -> ('a, (OBus_header.method_return * 'b) Lwt.t, 'b) OBus_type.ty_function -> 'a
  (** [send_message_with_reply connection header typ ...] Send a
      message and return a thread which wait for the reply *)

val ksend_message : ((t -> 'a OBus_header.t -> unit Lwt.t) -> 'c) -> ('b, 'c, unit) OBus_type.ty_function -> 'b
val ksend_message_with_reply : ((t -> OBus_header.method_call -> (OBus_header.method_return * 'b) Lwt.t) -> 'c) -> ('a, 'c, 'b) OBus_type.ty_function -> 'a
  (** Same thing but with continuation *)

(** {6 Sending dynamically-typed messages} *)

(** The following function are similar to the [send_*] function but
    instead of taking a type combinator they take a dynamically typed
    value.

    For example the two codes are equivalent:

    [send_message connection header [: int -> string -> unit ] 1 "toto"]

    and:

    {[
       usend_message connection header
            [Basic(Int32 1l);
             Basic(String "toto")]
    ]}
*)

type body = OBus_value.sequence

val dsend_message : t -> 'a OBus_header.t -> body -> unit Lwt.t
val dsend_message_with_reply : t -> OBus_header.method_call -> body ->
  (OBus_header.method_return * body) Lwt.t

(** {6 Helpers} *)

val method_call : t ->
  ?flags:OBus_header.flags ->
  ?sender:string ->
  ?destination:string ->
  path:OBus_path.t ->
  ?interface:string ->
  member:string ->
  ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> 'a

val kmethod_call : ((t -> 'a Lwt.t) -> 'b) ->
  ?flags:OBus_header.flags ->
  ?sender:string ->
  ?destination:string ->
  path:OBus_path.t ->
  ?interface:string ->
  member:string ->
  ('c, 'b, 'a) OBus_type.ty_function -> 'c

val dmethod_call : t ->
  ?flags:OBus_header.flags ->
  ?sender:string ->
  ?destination:string ->
  path:OBus_path.t ->
  ?interface:string ->
  member:string ->
  body -> body Lwt.t

val emit_signal : t ->
  ?flags:OBus_header.flags ->
  ?sender:string ->
  ?destination:string ->
  path:OBus_path.t ->
  interface:string ->
  member:string ->
  ('a, unit Lwt.t, unit) OBus_type.ty_function -> 'a

val demit_signal : t ->
  ?flags:OBus_header.flags ->
  ?sender:string ->
  ?destination:string ->
  path:OBus_path.t ->
  interface:string ->
  member:string ->
  body -> unit Lwt.t

val send_error : t -> OBus_header.method_call -> OBus_error.name -> OBus_error.message -> unit Lwt.t
  (** Send an error message in reply to a method call *)

val send_exn : t -> OBus_header.method_call -> exn -> unit Lwt.t
  (** [send_exn connection method_call exn] is a short-hand for
      passing [exn] through [OBus_error.unmake] then calling
      [send_error]. It raise an [Invalid_argument] if the exception is
      not registred as a DBus exception. *)

(** {6 Receiving signals} *)

type signal_receiver_id
  (** Id of a signal receiver *)

val add_signal_receiver : t ->
  ?sender:string ->
  ?path:OBus_path.t ->
  ?interface:string ->
  ?member:string ->
  ('a, unit, unit) OBus_type.ty_function -> (OBus_header.signal -> 'a) -> signal_receiver_id
  (** Add a signal receiver. [sender], [path], [interface] and
      [member] act as filters on the signal parameters.

      Note that with a message bus, you also need to add a matching
      rule. *)

val dadd_signal_receiver : t ->
  ?sender:string ->
  ?path:OBus_path.t ->
  ?interface:string ->
  ?member:string ->
  (OBus_header.signal -> body -> unit) -> signal_receiver_id
  (** Dynamically-typed version. This one is more generic than
      [add_signal_handler] since it does not put constraint on the
      signal signature. *)

val enable_signal_receiver : signal_receiver_id -> unit
val disable_signal_receiver : signal_receiver_id -> unit
  (** Enable/disable a signal receiver *)

val signal_receiver_enabled : signal_receiver_id -> bool

(** {6 Filters} *)

(** Filters are functions whose are applied on all incomming
    messages.

    Filters can be used to for debugging purpose or to write low-level
    DBus application (look at [samples/monitor.ml] to see an example
    of use of filters). *)

type filter_id
type filter = OBus_header.any -> body -> unit
  (** A filter will take as argument the header of the message and the
      body as q dynamically typed value *)

val add_filter : t -> filter -> filter_id
  (** [add_filter connection filter] add a filter to the given
      connection. *)

val enable_filter : filter_id -> unit
val disable_filter : filter_id -> unit
  (** Enable/disable a filter *)

val filter_enabled : filter_id -> bool

(** {6 Errors handling} *)

exception Protocol_error of string
  (** This exception is raised when an invalid DBus message is
      received. *)

(** Note: protocol and transport errors are considered as fatal
    errors. When a fatal error happen the connection is immediately
    closed. *)

val on_disconnect : t -> (exn -> unit) ref
  (** Function called when a fatal error happen. The default behaviour
      is to print an error message and to exit the program. *)
