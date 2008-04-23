(*
 * error.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of errors *)

type error =
  | Transport of Transport.error

exception Error of error * exn option
  (** An internal error of OBus, plus if any the native exception that
      make [Error] to be raised *)

exception DBus of string * string option
  (** Exception that can occur when calling a method on a dbus
      object. It contain the dbus error name and optionnally a error
      message.

      Note: generated interfaces can define their own exceptions
      corresponding to dbus well known errors. But when an unknwon
      error append, this exception is raised. *)
