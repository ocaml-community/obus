(*
 * oBus_path.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of dbus object paths *)

type t = string
    (** A complete path *)

type elt = string
    (** A path component *)

exception Invalid_path of string * string
  (** [Invalid_path(path, msg)] exception raised when an invalid path
      is used *)

(** {6 Validation} *)

val test : string -> string option
  (** Test if a string contain a valid path. Return [None] if it is
      the case or [Some msg] where is the explanation otherwise *)

val validate : string -> unit
  (** Validate a string. Raise an [Invalid_path] is the string does not
      contain a valid object path *)

(** {6 Construction} *)

val empty : t
  (** empty path *)

val append : t -> elt -> t
val (/) : t -> elt -> t
  (** [append path x] append [x] to [t]. [x] must not contain ['/'] *)

val make : elt list -> t
  (** make a path from a list of component *)

(** {6 utils} *)

val split : t -> elt list
  (** return all components of a path *)
