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

type unique = OBus_string.t
    (** Unique connection names. These names are valid for the
        lifetime of a message bus and are unique.

        example: ":1.1" *)

val test_unique : OBus_string.tester

type bus = OBus_string.t
    (** Bus names

        example: "org.freedesktop.DBus" *)

val test_bus : OBus_string.tester

type connection = OBus_string.t
    (** Either a unique name or a bus name *)

val test_connection : OBus_string.tester

type interface = OBus_string.t
    (** Interface names

        example: "org.freedesktop.DBus.Introspectable" *)

val test_interface : OBus_string.tester

type member = OBus_string.t
    (** Methods/signals/properties names

        example: "StartServiceByName" *)

val test_member : OBus_string.tester

type error = OBus_string.t
    (** Error names

        example: "org.freedesktop.Error.UnknownMethod" *)

val test_error : OBus_string.tester
