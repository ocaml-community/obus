(*
 * hal_manager.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
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

val on_device_added : (Hal_device.udi -> unit) -> OBus_signal.receiver Lwt.t
val on_device_removed : (Hal_device.udi -> unit) -> OBus_signal.receiver Lwt.t
val on_new_capability : (Hal_device.udi -> string -> unit) -> OBus_signal.receiver Lwt.t
val on_global_interface_lock_acquired : (string -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
val on_global_interface_lock_released : (string -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
