(*
 * error.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of errors *)

exception DBus of string * string
  (** A reply to a method call can be an error. When an error is
      received in response to a method call, and the error is not
      known (i.e. defined in an interface) this error is raised.

      Arguments are: the error name and the optionnal error
      message. *)

(**/**)

type error_maker = string -> string -> exn option
  (* An error maker is a function which take an error name and an
     error message and create an exception from it. This is for error
     message which come from a connection. *)

type error_unmaker = exn -> (string * string) option
  (* An error unmaker take an exception and return an error name and
     message. This is for service that raise exceptions. *)

val make_error : string -> string -> exn
  (* [make_error error_name error_message] create an exception from an
     error name and message. This is either a custom exception if a
     maker can build it or a [DBus] exception. *)

val unmake_error : exn -> (string * string) option
  (* [unmake_error exn] try to convert an exception into a DBus
     error. *)

val register_maker : error_maker -> unit
val register_unmaker : error_unmaker -> unit
