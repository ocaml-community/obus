(*
 * oBus_error.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus errors *)

type name = OBus_name.error
    (** An error name. It can contains multiple components, for
        example: "org.foo.bar.Error.Failed" *)

type message = string

exception DBus of name * message
  (** A reply to a method call can be an error. When an error is
      received in response to a method call, and the error is not
      known (i.e. it had not been previously registred) this error is
      raised.

      Arguments are: the error name and the error message. *)

(** {6 Well-known dbus exception} *)

(** This errors can be raised by any service. You can also raise them
    in a method your service implement.

    Note that the error message will normally be shown to the user so
    it must be explicative. *)

exception Unknown_method of message
  (** Raised when a method does not exists on a given object *)

exception Out_of_memory of message
  (** Raised when a call fail because the service does not have enough
      memory to satisfy the request *)

exception No_reply of message
  (** Raised when a method did not receive a reply, for example
      because of a timeout *)

(** {6 Exception registration} *)

val register : name -> (message -> exn) -> (exn -> message option) -> unit
  (** [register_exception name construct desctruct] register a
      exception as a DBus exception.

      [construct] is a function which take an error message and create
      an exception and [desctruct] take an exception and return an
      error message.

      An exception definition looks like:

      {[
        exception Caml_name of string
        let _ = OBus_error.register "Full.DBus.Name"
                  (fun msg -> Caml_name msg)
                  (function
                     | Caml_name -> Some msg
                     | _ -> None)
      ]}

      Or, with the syntax extension:

      {[
        exception Caml_name of OBus_error.message
          with obus("Full.DBus.Name")
      ]}
  *)

val make : name -> message -> exn
  (** Make an exception from a DBus error name and message. It return
      [DBus(name, message)] if [name] has not been registred *)

val unmake : exn -> (name * message) option
  (** Return the DBus error name and message of a caml exception. *)
