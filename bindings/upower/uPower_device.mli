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
(** Type of power source *)

type state =
  [ `Unknown
  | `Charging
  | `Discharging
  | `Empty
  | `Fully_charged
  | `Pending_charge
  | `Pending_discharge ]
(** The battery power state *)

type technology =
  [ `Unknown
  | `Lithium_ion
  | `Lithium_polymer
  | `Lithium_iron_phosphate
  | `Lead_acid
  | `Nickel_cadmium
  | `Nickel_metal_hydride ]
(** Technology used in the battery *)

val general_error : OBus_error.name

(** {6 Methods} *)

val get_statistics : t -> typ:string -> (float * float) list Lwt.t

val get_history :
  t ->
  typ:string ->
  timespan:int ->
  resolution:int ->
  (int * float * int) list Lwt.t

val refresh : t -> unit Lwt.t

(** {6 Signals} *)

val changed : t -> unit OBus_signal.t

(** {6 Properties} *)

val recall_url : t -> string OBus_property.r

val recall_vendor : t -> string OBus_property.r

val recall_notice : t -> bool OBus_property.r

val technology : t -> technology OBus_property.r

val capacity : t -> float OBus_property.r

val is_rechargeable : t -> bool OBus_property.r

val state : t -> state OBus_property.r

val is_present : t -> bool OBus_property.r

val percentage : t -> float OBus_property.r

val time_to_full : t -> int64 OBus_property.r

val time_to_empty : t -> int64 OBus_property.r

val voltage : t -> float OBus_property.r

val energy_rate : t -> float OBus_property.r

val energy_full_design : t -> float OBus_property.r

val energy_full : t -> float OBus_property.r

val energy_empty : t -> float OBus_property.r

val energy : t -> float OBus_property.r

val online : t -> bool OBus_property.r

val has_statistics : t -> bool OBus_property.r

val has_history : t -> bool OBus_property.r

val power_supply : t -> bool OBus_property.r

val typ : t -> typ OBus_property.r

val update_time : t -> int64 OBus_property.r

val serial : t -> string OBus_property.r

val model : t -> string OBus_property.r

val vendor : t -> string OBus_property.r

val native_path : t -> string OBus_property.r

val properties : t -> OBus_property.group
