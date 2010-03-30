(*
 * uPower_device.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

include OBus_proxy.Private

val get_statistics : t -> string -> (float * float) list Lwt.t
val get_history : t -> string -> int -> int -> (int * float * int) list Lwt.t
val refresh : t -> unit Lwt.t
val changed : t -> unit OBus_signal.t
val recall_url : t -> string Lwt.t
val set_recall_url : t -> string -> unit Lwt.t
val recall_vendor : t -> string Lwt.t
val set_recall_vendor : t -> string -> unit Lwt.t
val recall_notice : t -> bool Lwt.t
val set_recall_notice : t -> bool -> unit Lwt.t
val technology : t -> int Lwt.t
val set_technology : t -> int -> unit Lwt.t
val capacity : t -> float Lwt.t
val set_capacity : t -> float -> unit Lwt.t
val is_rechargeable : t -> bool Lwt.t
val set_is_rechargeable : t -> bool -> unit Lwt.t
val state : t -> int Lwt.t
val set_state : t -> int -> unit Lwt.t
val is_present : t -> bool Lwt.t
val set_is_present : t -> bool -> unit Lwt.t
val percentage : t -> float Lwt.t
val set_percentage : t -> float -> unit Lwt.t
val time_to_full : t -> int64 Lwt.t
val set_time_to_full : t -> int64 -> unit Lwt.t
val time_to_empty : t -> int64 Lwt.t
val set_time_to_empty : t -> int64 -> unit Lwt.t
val voltage : t -> float Lwt.t
val set_voltage : t -> float -> unit Lwt.t
val energy_rate : t -> float Lwt.t
val set_energy_rate : t -> float -> unit Lwt.t
val energy_full_design : t -> float Lwt.t
val set_energy_full_design : t -> float -> unit Lwt.t
val energy_full : t -> float Lwt.t
val set_energy_full : t -> float -> unit Lwt.t
val energy_empty : t -> float Lwt.t
val set_energy_empty : t -> float -> unit Lwt.t
val energy : t -> float Lwt.t
val set_energy : t -> float -> unit Lwt.t
val online : t -> bool Lwt.t
val set_online : t -> bool -> unit Lwt.t
val has_statistics : t -> bool Lwt.t
val set_has_statistics : t -> bool -> unit Lwt.t
val has_history : t -> bool Lwt.t
val set_has_history : t -> bool -> unit Lwt.t
val power_supply : t -> bool Lwt.t
val set_power_supply : t -> bool -> unit Lwt.t
val typ : t -> int Lwt.t
val set_typ : t -> int -> unit Lwt.t
val update_time : t -> int64 Lwt.t
val set_update_time : t -> int64 -> unit Lwt.t
val serial : t -> string Lwt.t
val set_serial : t -> string -> unit Lwt.t
val model : t -> string Lwt.t
val set_model : t -> string -> unit Lwt.t
val vendor : t -> string Lwt.t
val set_vendor : t -> string -> unit Lwt.t
val native_path : t -> string Lwt.t
val set_native_path : t -> string -> unit Lwt.t
