(*
 * oBus_name.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus names *)

(** For specific restrictions on DBus names, see
    @see <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-names> the specification

    General restrisction include:

    - names must not be empty
    - names must contains only ascii characters *)

type bus = OBus_string.t
    (** Bus names

        example: "org.freedesktop.DBus", ":1.1" *)

val validate_bus : OBus_string.validator

val is_unique : bus -> bool
  (** Tell wether a bus name is a unique connection name or not. *)

type interface = OBus_string.t
    (** Interface names

        example: "org.freedesktop.DBus.Introspectable" *)

val validate_interface : OBus_string.validator

type member = OBus_string.t
    (** Methods/signals/properties names

        example: "StartServiceByName" *)

val validate_member : OBus_string.validator

type error = OBus_string.t
    (** Error names

        example: "org.freedesktop.Error.UnknownMethod" *)

val validate_error : OBus_string.validator
