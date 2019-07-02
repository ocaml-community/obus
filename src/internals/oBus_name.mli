(*
 * oBus_name.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus names *)

(** For specific restrictions on D-Bus names, see
    @see <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-names> the specification

    General restrictions include:

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

(** {6 D-Bus name translation} *)

val split : string -> string list
  (** Split a name into longest blocks matched by the regular
      expression "[A-Z]*[^A-Z.]*":

      [split "SetCPUFreqGovernor" = ["Set"; "CPUFreq"; "Governor"]],
      [split "org.freedesktop.DBus" = ["org"; "freedesktop"; "DBus"]] *)

val ocaml_lid : string -> string
  (** Translate a D-Bus name into an ocaml-style lower-identifier:

      [caml_lid "SetCPUFreqGovernor" = "set_cpufreq_governor"] *)

val ocaml_uid : string -> string
  (** Translate a D-Bus name into an ocaml-style upper-identifier:

      [caml_uid "org.freedesktop.DBus" = "Org_freedesktop_dbus"] *)

val haskell_lid : string -> string
  (** Translate a D-Bus name into an haskell-style lower-identifier:

      [haskell_lid "SetCPUFreqGovernor" = "setCPUFreqGovernor"] *)

val haskell_uid : string -> string
  (** Translate a D-Bus name into an haskell-style upper-identifier:

      [haskell_uid "org.freedesktop.DBus" = "OrgFreedesktopDBus"] *)
