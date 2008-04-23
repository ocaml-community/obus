(*
 * address.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

exception Parse_error of string

type name = string
type key = string
type value = string

type raw = name * (key * value) list
    (** Type for a "raw" address *)

type t =
    [ `Unknown of string * (string * string) list ]
      (** Unknown address *)

val of_string : string -> [> t ] list
  (** [of_string str] parse [str] and return the list of transport
      addresses defined in it. It can raise a Parse_error. *)

val system : unit -> [> t ] list
  (** [system] list of addresses for system bus *)

val session : unit -> [> t ] list
  (** [session] list of addresses for session bus *)

type maker = raw -> [> t ] option
    (** Make an address from a raw just parsed address *)

val register_maker : maker -> unit
  (** [register_maker maker] Add a new address maker. *)
