(*
 * address.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type key = string
type value = string
type name = string

type parameter = key * value
type t = name * parameter list

exception Parse_error of string

val of_string : string -> t list
  (** [of_string str] parse [str] and return the list of transport
      defined in it *)

val system : unit -> t list
  (** [system_bus] list of addresses for system bus *)

val session : unit -> t list
  (** [session_bus] list of addresses for session bus *)
