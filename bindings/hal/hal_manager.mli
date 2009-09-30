(*
 * hal_manager.mli
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

val get_all_devices : unit -> Hal_device.udi list Lwt.t
val get_all_devices_with_properties : unit -> (Hal_device.udi * (string * Hal_device.property) list) list Lwt.t
val device_exists : Hal_device.udi -> bool Lwt.t
val find_device_string_match : string -> string -> Hal_device.udi list Lwt.t
val find_device_by_capability : string -> Hal_device.udi list Lwt.t
val new_device : unit -> string Lwt.t
val remove : string -> unit Lwt.t
val commit_to_gdl : string -> string -> unit Lwt.t
val acquire_global_interface_lock : string -> bool -> unit Lwt.t
val release_global_interface_lock : string -> unit Lwt.t
val singleton_addon_is_ready : string -> unit Lwt.t

val device_added : unit -> Hal_device.udi OBus_signal.t Lwt.t
val device_removed : unit -> Hal_device.udi OBus_signal.t Lwt.t
val new_capability : unit -> (Hal_device.udi * string) OBus_signal.t Lwt.t
val global_interface_lock_acquired : unit -> (string * string * int) OBus_signal.t Lwt.t
val global_interface_lock_released : unit -> (string * string * int) OBus_signal.t Lwt.t
