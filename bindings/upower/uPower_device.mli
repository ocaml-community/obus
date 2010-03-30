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

val recall_url : t -> (string, [ `readable | `writable ]) OBus_property.t
val recall_vendor : t -> (string, [ `readable | `writable ]) OBus_property.t
val recall_notice : t -> (bool, [ `readable | `writable ]) OBus_property.t
val technology : t -> (int, [ `readable | `writable ]) OBus_property.t
val capacity : t -> (float, [ `readable | `writable ]) OBus_property.t
val is_rechargeable : t -> (bool, [ `readable | `writable ]) OBus_property.t
val state : t -> (int, [ `readable | `writable ]) OBus_property.t
val is_present : t -> (bool, [ `readable | `writable ]) OBus_property.t
val percentage : t -> (float, [ `readable | `writable ]) OBus_property.t
val time_to_full : t -> (int64, [ `readable | `writable ]) OBus_property.t
val time_to_empty : t -> (int64, [ `readable | `writable ]) OBus_property.t
val voltage : t -> (float, [ `readable | `writable ]) OBus_property.t
val energy_rate : t -> (float, [ `readable | `writable ]) OBus_property.t
val energy_full_design : t -> (float, [ `readable | `writable ]) OBus_property.t
val energy_full : t -> (float, [ `readable | `writable ]) OBus_property.t
val energy_empty : t -> (float, [ `readable | `writable ]) OBus_property.t
val energy : t -> (float, [ `readable | `writable ]) OBus_property.t
val online : t -> (bool, [ `readable | `writable ]) OBus_property.t
val has_statistics : t -> (bool, [ `readable | `writable ]) OBus_property.t
val has_history : t -> (bool, [ `readable | `writable ]) OBus_property.t
val power_supply : t -> (bool, [ `readable | `writable ]) OBus_property.t
val typ : t -> (int, [ `readable | `writable ]) OBus_property.t
val update_time : t -> (int64, [ `readable | `writable ]) OBus_property.t
val serial : t -> (string, [ `readable | `writable ]) OBus_property.t
val model : t -> (string, [ `readable | `writable ]) OBus_property.t
val vendor : t -> (string, [ `readable | `writable ]) OBus_property.t
val native_path : t -> (string, [ `readable | `writable ]) OBus_property.t
