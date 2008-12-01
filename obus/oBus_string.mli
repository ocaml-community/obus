(*
 * oBus_string.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Restriction on strings used with DBus *)

(** There are a lot a various restrictions for strings used in DBus.
    Obus only verify strings when a message is sent or received *)

type error = {
  (** Contains informations about invalid strings *)

  typ : string;
    (** Type of string ("string", "bus name", "error name", "path",
        ...) *)

  str : string;
  (** The string which fail to validate *)

  ofs : int;
  (** is the position in byte where the validation failed *)

  msg : string;
  (** explain why the string failed to validate *)
}

val error_message : error -> string
  (** [error_message error] return a human-readabe error message *)

type validator = string -> error option
  (** Function which test if a string is correct.

      - if it is, returns [None]
      - if not, returns [Some(ofs, msg)] *)

exception Invalid_string of error

val assert_validate : validator -> string -> unit
  (** Raises {!Invalid_string} if the given string failed to
      validate *)

val lwt_assert_validate : validator -> string -> unit Lwt.t
  (** Lwt-version of {!assert_validate} *)

(** {6 Common strings} *)

type t = string
    (** Type for common strings, restrictions are:

        - a string must be encoded in valid UTF-8
        - a string must not contains the null byte *)

val validate : validator
  (** Validatition function for commong strings *)
