(*
 * oBus_path.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of dbus object paths *)

type element = string
    (** A path component *)

type t = element list
   (** A complete path *)

(** {6 Construction} *)

val empty : t
  (** Empty path *)

val after : t -> t -> t option
  (** [after prefix path] if [path = prefix @ p] return [Some p], and
      [None] if not *)

val of_string : string -> t
  (** Create an object path from a string.

      @raise Invalid_path if the given string does not represent a
      valid object path *)

val to_string : t -> string
  (** Return a string representation of an object path *)

val escape : string -> element
  (** Escape an arbitrary string into a valid element *)

val unescape : element -> string

(** {6 Validation} *)

val test_element : string -> string option
  (** Test weather a string is a valid path element.

      @return [None] if it the case or [Some msg] where [msg] is the
      explanation otherwise *)

val test : string -> string option
  (** Same thing but for a string containing a whole object path *)

exception Invalid_element of string * string
  (** [Invalid_element(element, msg)] exception raised when an invalid
      path element is used *)

val validate_element : string -> unit
  (** Validate a path element.

      @raise Invalid_element if the string is not a valid path
      element *)

exception Invalid_path of string * string
  (** [Invalid_path(path, msg)] exception raised when an invalid path
      is used *)

val validate : string -> unit
  (** Validate a path.

      @raise Invalid_path if the string does not represent a valid
      object path *)
