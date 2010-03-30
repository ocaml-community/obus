(*
 * hal_device.mli
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Hal devices *)

include OBus_proxy.Private

type udi = OBus_path.t
    (** Unique Device Identifier *)

val obus_udi : udi OBus_type.basic

val udi : t -> udi
  (** Return the udi of a device *)

val computer : unit -> t Lwt.t
  (** The computer device *)

type property =
    (** A device property *)
  | Pstring of string
      (** An UTF8 string *)
  | Pstrlist of string list
      (** List of UTF8 strings *)
  | Pint of int32
      (** 32-bit signed integer *)
  | Puint64 of int64
      (** 64-bit unsigned integer *)
  | Pbool of bool
  | Pdouble of float
      (** IEEE754 double precision floating point number  *)

val obus_property : property OBus_type.container

(** {6 Common device interface} *)

val get_all_properties : t -> (string * property) list Lwt.t
val set_multiple_properties : t -> (string * property) list -> unit Lwt.t
val get_property : t -> string -> property Lwt.t
val get_property_string : t -> string -> string Lwt.t
val get_property_string_list : t -> string -> string list Lwt.t
val get_property_integer : t -> string -> int Lwt.t
val get_property_boolean : t -> string -> bool Lwt.t
val get_property_double : t -> string -> float Lwt.t
val set_property : t -> string -> property -> unit Lwt.t
val set_property_string : t -> string -> string -> unit Lwt.t
val set_property_string_list : t -> string -> string list -> unit Lwt.t
val set_property_integer : t -> string -> int -> unit Lwt.t
val set_property_boolean : t -> string -> bool -> unit Lwt.t
val set_property_double : t -> string -> float -> unit Lwt.t
val remove_property : t -> string -> unit Lwt.t
val get_property_type : t -> string -> int Lwt.t
val property_exists : t -> string -> bool Lwt.t
val add_capability : t -> string -> unit Lwt.t
val query_capability : t -> string -> bool Lwt.t
val lock : t -> string -> bool Lwt.t
val unlock : t -> bool Lwt.t
val acquire_interface_lock : t -> string -> bool -> unit Lwt.t
val release_interface_lock : t -> string -> unit Lwt.t
val is_caller_locked_out : t -> string -> string -> bool Lwt.t
val is_caller_privileged : t -> string -> string list -> string -> string Lwt.t
val is_locked_by_others : t -> string -> bool Lwt.t
val string_list_append : t -> string -> string -> unit Lwt.t
val string_list_prepend : t -> string -> string -> unit Lwt.t
val string_list_remove : t -> string -> string -> unit Lwt.t
val emit_condition : t -> string -> string -> bool Lwt.t
val rescan : t -> bool Lwt.t
val reprobe : t -> bool Lwt.t
val claim_interface : t -> string -> string -> bool Lwt.t
val addon_is_ready : t -> bool Lwt.t

val property_modified : t -> (int * (string * bool * bool) list) OBus_proxy.signal
val condition : t -> (string * string) OBus_proxy.signal
val interface_lock_acquired : t -> (string * string * int) OBus_proxy.signal
val interface_lock_released : t -> (string * string * int) OBus_proxy.signal

(** {6 Specifics interfaces} *)

module Volume : sig
  val mount : t -> string -> string -> string list -> int Lwt.t
  val unmount : t -> string list -> int Lwt.t
  val eject : t -> string list -> int Lwt.t
end

module Storage : sig
  val eject : t -> string list -> int Lwt.t
  val close_tray : t -> string list -> int Lwt.t
end

module Storage_removable : sig
  val check_for_media : t -> bool Lwt.t
end

module Wake_on_lan : sig
  val get_supported : t -> int Lwt.t
  val get_enabled : t -> int Lwt.t
  val set_enabled : t -> bool -> int Lwt.t
end

module System_power_management : sig
  val suspend : t -> int -> int Lwt.t
  val suspend_hybrid : t -> int -> int Lwt.t
  val hibernate : t -> int Lwt.t
  val shutdown : t -> int Lwt.t
  val reboot : t -> int Lwt.t
  val set_power_save : t -> bool -> int Lwt.t
end

module Cpufreq : sig
  val set_cpufreq_governor : t -> string -> unit Lwt.t
  val set_cpufreq_performance : t -> int -> unit Lwt.t
  val set_cpufreq_consider_nice : t -> bool -> unit Lwt.t
  val get_cpufreq_governor : t -> string Lwt.t
  val get_cpufreq_performance : t -> int Lwt.t
  val get_cpufreq_consider_nice : t -> bool Lwt.t
  val get_cpufreq_available_governors : t -> string list Lwt.t
end

module Laptop_panel : sig
  val set_brightness : t -> int -> int Lwt.t
  val get_brightness : t -> int Lwt.t
end

module Dock_station : sig
  val undock : t -> int Lwt.t
end

module Kill_switch : sig
  val set_power : t -> bool -> int Lwt.t
  val get_power : t -> int Lwt.t
end
