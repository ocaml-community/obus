(*
 * address.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of DBus addresses *)

type name = string
type key = string
type value = string
type guid = string

type raw = name * (key * value) list
    (** A just parsed address *)

type known =
    (** Addresses handled by obus *)
  | Unix of string
      (** A unix socket, the argument is the path *)
  | Unknown

type t = raw * known * guid option

exception Parse_error of string

val of_string : string -> t list
  (** [of_string str] parse [str] and return the list of addresses
      defined in it. It can raise a [Parse_error]. *)

val system : unit -> t list
  (** [system] list of addresses for system bus *)

val session : unit -> t list
  (** [session] list of addresses for session bus *)
