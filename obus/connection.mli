(*
 * connection.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus connection *)

(** A DBus connection is a channel opened with another application
    which also implement the DBus protocol. It is used to exchange
    DBus messages. *)

type t
  (** Abstract type for a connection *)

type name = string
    (** A connection unique name. Only used in conjonction with a
        message bus. *)

type guid = Address.guid
    (** Unique identifier of a server *)

(** {6 Creation} *)

val of_transport : ?shared:bool -> Transport.t -> t
  (** [of_transport shared transport] create a dbus connection over
      the given transport. If [shared] is true and a connection to the
      same server is already open, then it is used instead of
      [transport], this is the default behaviour. *)

val of_authenticated_transport : ?shared:bool -> Transport.t -> guid -> t
  (** Same as of_transport but assume that the authentification is
      done. *)

val of_addresses : ?shared:bool -> Address.t list -> t
  (** [of_addresses shared addresses] shorthand for obtaining
      transport and doing [of_transport] *)

(** Note: if using threads, any of the above function will
    automatically start a thread which will dispatch the messages for
    the newly created connection. *)

val close : t -> unit
  (** Close a connection. All other functions will raise a
      [Invalid_argument] if you try to do something with a closed
      connection.

      Note: when a connection is closed, the transport it use is
      closed too. *)

(** {6 Informations} *)

val transport : t -> Transport.t
  (** [transport connection] get the transport associated with a
      connection *)

val guid : t -> guid
  (** [guid connection] return the unique identifier of the server at
      the other side of the connection *)

(** {6 Sending messages} *)

type body = Values.values

val send_message : t -> Header.t -> body -> unit
  (** Send a message. do not wait for the reply. *)

val send_message_sync : t -> Header.method_call -> body -> Header.method_return * body
  (** Send a message and wait for the reply.

      If the reply is an error, an exception will be raised: if the
      exception is known (like [DBus.Error.Failed]) it will be
      raised. If not an [Error.DBus] will be raised. *)

val send_message_async : t -> Header.method_call -> body -> ?on_error:(exn -> unit) -> (Header.method_return -> body -> unit) -> unit
  (** Same as send_message but return immediatly and register a
      function which will receive the result.

      If the reply is an error, [on_error] will be called on it. If
      [on_error] is not provided and the reply is an error then it
      will just be dropped. *)

type 'a cookie
  (** See {!Cookie} for details *)

val send_message_cookie : t -> Header.method_call -> body -> (Header.method_return * body) cookie
  (** Send a message an return a cookie for later retrieval of the
      result *)

val send_exn : t -> Header.method_call -> exn -> unit
  (** Send an error message in reply to a method call. It raise an
      [Invalid_argument] if the exception is not a DBus error. *)

val send_error : t -> Header.method_call -> string -> string -> unit
  (** [send_error connection method_call name message] same as
      [send_exn] but take a name and a message instead of an
      exception. *)

(** {6 Dispatching} *)

type filter_id

type filter = Header.t -> body -> unit
  (** Filters are used to filter all incoming message which come from
      a connection. *)

val add_filter : t -> filter -> filter_id
  (** [add_filter connection filter] add a filter to the
      connection. This filter will be called before all previously
      defined filter. *)

val remove_filter : t -> filter_id -> unit
  (** Remove the given filter. It do nothing if the filter as already
      been removed.

      Note: it is not guaranteed that the the filter will not be
      called again. Especially if using thread or if [remove_filter]
      is called in a filter then it is possible that the handler is
      called on messages currently being dispatched. *)

val dispatch : t -> unit
  (** [dispatch connection] read and dispatch one message. It has only
      effect in the dispatcher thread (if not using threads this is
      always the case, if using threads this is only the case in a
      filter or an handler). *)

(** {6 Errors handling} *)

exception Protocol_error of string
  (** This exception is raised when an invalid DBus message is
      received. *)

(** Note: protocol and transport errors are considered as fatal
    errors. When a fatal error happen the connection is immediately
    closed. So when a [Protocol_error] or a [Transport.Error] is
    raised by any of the function of this module the connection as
    already been closed. *)

exception Cannot_send of string
  (** This exception is raised when a message can not be send. The
      only possible reason is actually because it contain too big
      datas (see [Info] for limits).

      This error can be raised by any of the function [send_*]. *)

val on_error : t -> (exn -> bool) -> unit
  (** This function is used to handle uncaugth exception on the
      dispatcher thread (if not using thread it will never be used).

      The default handler will just print an error message and exit
      the program with [Pervasives.exit].

      If the handler raise itself an exception, the default handler
      will be called.

      If the error is a fatal error, then the connection will be
      closed before the handler is called and the dispatcher thread
      will terminate after it.

      If not the handler can choose to ignore the error. In this case
      the result of the handler should be:

      [true] if the dispatcher must continue to run.

      [false] if the dispatcher must terminate. In this case the
      connection will be closed. *)

(** Note: if a fatal error happen in another thread than the
    dispatcher (the only possible reason is a transport error) then
    the dispatcher will simply exit without doing anything else when
    it will see it. *)

(**/**)

open Wire
type intern_method_call_handler_result =
  | Intern_mchr_no_such_method
  | Intern_mchr_no_such_object
  | Intern_mchr_ok of (unit -> unit) Wire.body_reader
val intern_send_message : t -> Header.t -> body_writer -> unit
val intern_send_message_sync : t -> Header.method_call -> body_writer -> (Header.method_return -> 'a body_reader) -> 'a
val intern_send_message_async : t -> Header.method_call -> body_writer -> ?on_error:(exn -> unit) -> (Header.method_return -> (unit -> unit) body_reader) -> unit
val intern_send_message_cookie : t -> Header.method_call -> body_writer -> (Header.method_return -> 'a body_reader) -> 'a cookie
val intern_add_signal_handler : t -> Interface.name -> (Header.signal -> (unit -> unit) body_reader option) -> unit
val intern_add_method_call_handler : t -> Interface.name -> (Header.method_call -> intern_method_call_handler_result) -> unit
val intern_get_name : t -> (unit -> string) -> string
val intern_cookie_get : 'a cookie -> 'a
val intern_cookie_is_ready : 'a cookie -> bool
val intern_cookie_is_value : 'a cookie -> bool
val intern_cookie_is_exn : 'a cookie -> bool
val intern_cookie_get_if_ready : 'a cookie -> 'a option

