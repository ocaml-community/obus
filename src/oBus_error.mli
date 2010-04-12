(*
 * oBus_error.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus errors *)

type name = OBus_name.error
    (** An error name. For example: ["org.foo.bar.Error.Failed"] *)

type message = string
    (** An error message *)

exception DBus of exn * name * message
  (** A reply to a method call can be an error. When an error is
      received in response to a method call, this error is raised.

      Arguments are:
      - an exception, used to match on the error
      - the D-Bus error name
      - the error message

      The first argument is only used for pattern-matching, to avoid
      matching on the error name. The error should have been
      registered before. If the error is not register, {!Unknown} is
      used. *)

exception Unknown
  (** Unknown D-Bus error *)

exception OCaml
  (** Exception used for uncaught exceptions *)

(** {6 Registration} *)

val register : name : name -> exn : exn -> unit
  (** [register ~name ~exn] registers the exception [exn] for the
      D-Bus error [name] *)

val name_of_exn : exn -> name
  (** [name_of_exn exn] returns the name associated with the given
      exception. It raises [Not_found] if the given exception is not
      registered. *)

val exn_of_name : name -> exn
  (** [name_of_exn exn] returns the exception associated with the
      given name. It raises [Not_found] if the given name is not
      registered. *)

(** {6 Raising D-Bus errors} *)

val make : exn -> message -> exn
  (** [make exn message] creates an exception [DBus(exn, name,
      message)] where [name] is the name under which [exn] is
      registered.

      If [exn] is not registered, it raises [Not_found]. *)

val make_by_name : name -> message -> exn
  (** Same as {!make} but takes the argument name instead of the key
      exception. If the given error is not registered, it uses
      {!Unknown}. *)

val raise : exn -> message -> 'a
  (** [raise exn message] is a short-hand for [raise (make exn
      message)] *)

val fail : exn -> message -> 'a Lwt.t
  (** [fail exn message] is a short-hand for [fail (make exn
      message)] *)

(** {6 Well-known dbus exception} *)

(** This errors can be raised by any service. You can also raise them
    in a method your service implement.

    Note that the error message will normally be shown to the user so
    it must be explicative. *)

exception Failed
  (** "org.freedesktop.DBus.Error.Failed" *)

exception Invalid_args
  (** "org.freedesktop.DBus.Error.InvalidArgs" *)

exception Unknown_method
  (** "org.freedesktop.DBus.Error.UnknownMethod" *)

exception No_memory
  (** "org.freedesktop.DBus.Error.NoMemory" *)

exception No_reply
  (** "org.freedesktop.DBus.Error.NoReply" *)
