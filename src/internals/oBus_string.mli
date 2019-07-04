(*
 * oBus_string.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Restriction on strings used with D-Bus *)

(** There are a lot of restrictions for strings used in D-Bus.
    OBus only verifies strings when a message is sent or received *)

type error = {
  (** Contains informations about invalid strings *)

  typ : string;
  (** Type of string ("string", "bus name", "error name", "path",
      ...) *)

  str : string;
  (** The string which fail to validate *)

  ofs : int;
  (** is the position in bytes where the validation failed *)

  msg : string;
  (** explains why the string failed to validate *)
}

val error_message : error -> string
  (** [error_message error] returns a human-readable error message *)

(** {8 Error projections} *)

val typ : error -> string
val str : error -> string
val ofs : error -> int
val msg : error -> string

(** {6 Validators} *)

type validator = string -> error option
  (** Tests if a string is correct.

      - if it is, returns [None]
      - if not, returns [Some(ofs, msg)] *)

exception Invalid_string of error

val assert_validate : validator -> string -> unit
  (** Raises {!Invalid_string} if the given string failed to
      validate *)

(** {6 Common strings} *)

type t = string
    (** Type for common strings, restrictions are:

        - a string must be encoded in valid UTF-8
        - a string must not contains the null byte *)

val validate : validator
  (** Validation function for common strings *)
