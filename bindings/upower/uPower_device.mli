(*
 * uPower_device.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** UPower device interface *)

include OBus_proxy.Private

(** {6 Types} *)

(** Type of power source *)
type typ =
    [ `Unknown
    | `Line_power
    | `Battery
    | `Ups
    | `Monitor
    | `Mouse
    | `Keyboard
    | `Pda
    | `Phone ]

(** The battery power state *)
type state =
    [ `Unknown
    | `Charging
    | `Discharging
    | `Empty
    | `Fully_charged
    | `Pending_charge
    | `Pending_discharge ]

(** Technology used in the battery *)
type technology =
    [ `Unknown
    | `Lithium_ion
    | `Lithium_polymer
    | `Lithium_iron_phosphate
    | `Lead_acid
    | `Nickel_cadmium
    | `Nickel_metal_hydride ]

(** {6 Methods} *)

val get_statistics : t -> string -> (float * float) list Lwt.t
val get_history : t -> string -> int -> int -> (int * float * int) list Lwt.t
val refresh : t -> unit Lwt.t

(** {6 Signals} *)

val changed : t -> unit OBus_signal.t

(** {6 Properties} *)

val recall_url : t -> string OBus_property.rw
val recall_vendor : t -> string OBus_property.rw
val recall_notice : t -> bool OBus_property.rw
val technology : t -> technology OBus_property.rw
val capacity : t -> float OBus_property.rw
val is_rechargeable : t -> bool OBus_property.rw
val state : t -> state OBus_property.rw
val is_present : t -> bool OBus_property.rw
val percentage : t -> float OBus_property.rw
val time_to_full : t -> int64 OBus_property.rw
val time_to_empty : t -> int64 OBus_property.rw
val voltage : t -> float OBus_property.rw
val energy_rate : t -> float OBus_property.rw
val energy_full_design : t -> float OBus_property.rw
val energy_full : t -> float OBus_property.rw
val energy_empty : t -> float OBus_property.rw
val energy : t -> float OBus_property.rw
val online : t -> bool OBus_property.rw
val has_statistics : t -> bool OBus_property.rw
val has_history : t -> bool OBus_property.rw
val power_supply : t -> bool OBus_property.rw
val typ : t -> typ OBus_property.rw
val update_time : t -> int64 OBus_property.rw
val serial : t -> string OBus_property.rw
val model : t -> string OBus_property.rw
val vendor : t -> string OBus_property.rw
val native_path : t -> string OBus_property.rw
