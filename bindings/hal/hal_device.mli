(*
 * hal_device.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type udi = OBus_path.t
    (** A device unique identifier *)

type property =
    (** A device property *)
  | Pstring of string
      (** A UTF8 string *)
  | Pstrlist of string list
      (** List of UTF8 strings *)
  | Pint of int32
      (** 32-bit signed integer *)
  | Puint64 of int64
      (** 64-bit unsigned integer *)
  | Pbool of bool
  | Pdouble of float
      (** IEEE754 double precision floating point number  *)

val ob_property : (property, _, OBus_types.dvariant) OBus_comb.one
val ob_udi : (udi, _, OBus_types.dobject_path) OBus_comb.one

val computer : udi
  (** The computer device *)

(** {6 Common device interface} *)

val get_all_properties : udi -> (string * property) list Lwt.t
val set_multiple_properties : udi -> (string * property) list -> unit Lwt.t
val get_property : udi -> string -> property Lwt.t
val get_property_string : udi -> string -> string Lwt.t
val get_property_string_list : udi -> string -> string list Lwt.t
val get_property_integer : udi -> string -> int Lwt.t
val get_property_boolean : udi -> string -> bool Lwt.t
val get_property_double : udi -> string -> float Lwt.t
val set_property : udi -> string -> property -> unit Lwt.t
val set_property_string : udi -> string -> string -> unit Lwt.t
val set_property_string_list : udi -> string -> string list -> unit Lwt.t
val set_property_integer : udi -> string -> int -> unit Lwt.t
val set_property_boolean : udi -> string -> bool -> unit Lwt.t
val set_property_double : udi -> string -> float -> unit Lwt.t
val remove_property : udi -> string -> unit Lwt.t
val get_property_type : udi -> string -> int Lwt.t
val property_exists : udi -> string -> bool Lwt.t
val add_capability : udi -> string -> unit Lwt.t
val query_capability : udi -> string -> bool Lwt.t
val lock : udi -> string -> bool Lwt.t
val unlock : udi -> bool Lwt.t
val acquire_interface_lock : udi -> string -> bool -> unit Lwt.t
val release_interface_lock : udi -> string -> unit Lwt.t
val is_caller_locked_out : udi -> string -> string -> bool Lwt.t
val is_caller_privileged : udi -> string -> string list -> string -> string Lwt.t
val is_locked_by_others : udi -> string -> bool Lwt.t
val string_list_append : udi -> string -> string -> unit Lwt.t
val string_list_prepend : udi -> string -> string -> unit Lwt.t
val string_list_remove : udi -> string -> string -> unit Lwt.t
val emit_condition : udi -> string -> string -> bool Lwt.t
val rescan : udi -> bool Lwt.t
val reprobe : udi -> bool Lwt.t
val claim_interface : udi -> string -> string -> bool Lwt.t
val addon_is_ready : udi -> bool Lwt.t

(** {6 Specifics interfaces} *)

module Volume : sig
  val mount : udi -> string -> string -> string list -> int Lwt.t
  val unmount : udi -> string list -> int Lwt.t
  val eject : udi -> string list -> int Lwt.t
end

module Storage : sig
  val eject : udi -> string list -> int Lwt.t
  val close_tray : udi -> string list -> int Lwt.t
end

module Storage_removable : sig
  val check_for_media : udi -> bool Lwt.t
end

module Wake_on_lan : sig
  val get_supported : udi -> int Lwt.t
  val get_enabled : udi -> int Lwt.t
  val set_enabled : udi -> bool -> int Lwt.t
end

module System_power_management : sig
  val suspend : udi -> int -> int Lwt.t
  val suspend_hybrid : udi -> int -> int Lwt.t
  val hibernate : udi -> int Lwt.t
  val shutdown : udi -> int Lwt.t
  val reboot : udi -> int Lwt.t
  val set_power_save : udi -> bool -> int Lwt.t
end

module Cpufreq : sig
  val set_cpufreq_governor : udi -> string -> unit Lwt.t
  val set_cpufreq_performance : udi -> int -> unit Lwt.t
  val set_cpufreq_consider_nice : udi -> bool -> unit Lwt.t
  val get_cpufreq_governor : udi -> string Lwt.t
  val get_cpufreq_performance : udi -> int Lwt.t
  val get_cpufreq_consider_nice : udi -> bool Lwt.t
  val get_cpufreq_available_governors : udi -> string list Lwt.t
end

module Laptop_panel : sig
  val set_brightness : udi -> int -> int Lwt.t
  val get_brightness : udi -> int Lwt.t
end

module Dock_station : sig
  val undock : udi -> int Lwt.t
end

module Kill_switch : sig
  val set_power : udi -> bool -> int Lwt.t
  val get_power : udi -> int Lwt.t
end
