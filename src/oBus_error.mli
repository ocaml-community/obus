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

exception DBus of name * message
  (** A reply to a method call can be an error. When an error is
      received in response to a method call, this error is raised.

      Arguments are:
      - the D-Bus error name
      - the error message
  *)

val ocaml : name
  (** Exception used for uncaught exceptions *)

(** {6 Raising D-Bus errors} *)

val make : name -> message -> exn
  (** [make exn message] creates an exception [DBus(exn, name,
      message)] where [name] is the name under which [exn] is
      registered.

      If [exn] is not registered, it raises [Not_found]. *)

val raise : name -> message -> 'a
  (** [raise exn message] is a short-hand for [raise (make exn
      message)] *)

val fail : name -> message -> 'a Lwt.t
  (** [fail exn message] is a short-hand for [fail (make exn
      message)] *)

(** {6 Well-known dbus exception} *)

(** This errors can be raised by any service. You can also raise them
    in a method your service implement.

    Note that the error message will normally be shown to the user so
    it must be explicative. *)

val failed : name
  (** "org.freedesktop.DBus.Error.Failed" *)

val invalid_args : name
  (** "org.freedesktop.DBus.Error.InvalidArgs" *)

val unknown_method : name
  (** "org.freedesktop.DBus.Error.UnknownMethod" *)

val no_memory : name
  (** "org.freedesktop.DBus.Error.NoMemory" *)

val no_reply : name
  (** "org.freedesktop.DBus.Error.NoReply" *)
