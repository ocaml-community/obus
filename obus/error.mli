(*
 * error.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of errors *)

exception DBus of string * string option
  (** A reply to a method call can be an error. When an error is
      received in response to a method call, and the error is not
      known (i.e. defined in an interface) this error is raised.

      Arguments are: the error name and the optionnal error
      message. *)

val to_string : exn -> string
  (** Return a description of any exception raised by OBus. If it is
      not one of them just return the empty string. *)

(**/**)

val get_error : Header.recv -> Wire.buffer -> Wire.ptr -> string * string option
  (* retrn the error name and message of a dbus error message *)

val raise_error : Header.recv -> Wire.buffer -> Wire.ptr -> 'a
  (* get the name and message of a dbus error message and raide a
     [DBus] exception *)
