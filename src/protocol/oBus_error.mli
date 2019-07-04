(*
 * oBus_error.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus errors management *)

(** This module integrates D-Bus errors into OCaml exceptions, and
    OCaml exceptions into D-Bus errors.

    To do this, an OCaml exception that maps a D-Bus error should be
    registered with {!Register}. *)

type name = OBus_name.error
    (** An error name. For example: ["org.foo.bar.Error.Failed"] *)

type message = string
    (** An error message *)

exception DBus of name * message
  (** General exception for D-Bus errors. When the reply to a method
      call is a D-Bus error that have not been registered, this
      exception is raised.

      Arguments are:
      - the D-Bus error name
      - the error message
  *)

val ocaml : name
  (** The name of the D-Bus error which is generated for uncaught
      ocaml exceptions that have not been registered *)

(** {6 D-Bus errors creating/casting} *)

val name : exn -> name
  (** [name exn] returns the D-Bus error name under which this
      exception is registered. If the exception is not registered,
      then [ocaml] is returned. *)

val make : name -> message -> exn
  (** [make exn message] creates an exception from an error name and
      an error message. If the name is not registered, then
      [DBus(name, message)] is returned. *)

val cast : exn -> name * message
  (** [cast exn] returns the D-Bus name and message of the given
      exception. If the exception is not registered, [(ocaml,
      Printexc.to_string exn)] is returned. *)

(** {6 Errors registration} *)

(** Signature for D-Bus error *)
module type Error = sig
  exception E of string
    (** The OCaml exception for this error *)

  val name : name
    (** The D-Bus name if this error *)
end

module Register(Error : Error) : sig end
  (** Register an error. The typical use of the functor is:

      {[
        exception My_exception of string
        let module M =
          OBus_error.Register(struct
                                exception E = My_exception
                                let name = "my.exception.name"
                              end)
        in ()
      ]}

      But you can also write this with the syntax extension:

      {[
        exception My_exception of string
          [@@obus "my.exception.name"]
      ]}
  *)

(** {6 Well-known dbus exception} *)

(** The following errors can be raised by any service. You can also
    raise them in a method your service implement.

    Note that the error message will normally be shown to the user so
    they must be explicative. *)

exception Failed of message
  (** The [org.freedesktop.DBus.Error.Failed] error *)

exception Invalid_args of message
  (** The [org.freedesktop.DBus.Error.InvalidArgs] error *)

exception Unknown_method of message
  (** The [org.freedesktop.DBus.Error.UnknownMethod] error *)

exception Unknown_object of message
  (** The [org.freedesktop.DBus.Error.UnknownObject] error *)

exception Unknown_interface of message
  (** The [org.freedesktop.DBus.Error.UnknownInterface] error *)

exception Unknown_property of message
  (** The [org.freedesktop.DBus.Error.UnknownProperty] error *)

exception Property_read_only of message
  (** The [org.freedesktop.DBus.Error.PropertyReadOnly] error *)

exception No_memory of message
  (** The [org.freedesktop.DBus.Error.NoMemory] error *)

exception No_reply of message
  (** The [org.freedesktop.DBus.Error.NoReply] error *)
