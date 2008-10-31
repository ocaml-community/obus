(*
 * oBus_name.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus names construction/validation *)

(** For specific restrictions on DBus names, see
    @see <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-names> the specification *)

exception Invalid_name of string * string * string
  (** [Invalid_name(typ, name, msg)] Exception raised when a name is
      not a valid name. [typ] is the expected type of the name, [name]
      is the string containing the name which fail to validate, and
      [msg] explain why it is not a valid name *)

(** Operation on names *)
module type Name = sig
  type t = string

  val test : string -> string option
    (** Verify that a string contain a valid name. Return [None] if it
        is the case or [Some msg] where [msg] explain why the string
        is not a valid name *)

  val validate : string -> unit
    (** Validate the given string.
        @raise Invalid_name if the string does not contain a valid
        name *)
end

module Unique : Name
  (** Unique connection names. These names are valid for the lifetime
      of a message bus and are unique.

      example: ":1.1" *)

module Bus : Name
  (** Bus names

      example: "org.freedesktop.DBus" *)

module Connection : Name
  (** Either a unique names or a bus names *)

module Interface : Name
  (** Interface names

      example: "org.freedesktop.DBus.Introspectable" *)

module Member : Name
  (** Methods/signals/properties names

      example: "StartServiceByName" *)

module Error : Name
  (** Error names

      example: "org.freedesktop.Error.UnknownMethod" *)

type unique = Unique.t
type bus = Bus.t
type connection = Connection.t
type interface = Interface.t
type member = Member.t
type error = Error.t
