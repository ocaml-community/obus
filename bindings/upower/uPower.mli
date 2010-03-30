(*
 * uPower.mli
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

include OBus_peer.Private

val daemon : unit -> t Lwt.t
  (** [daemon ()] returns the peer object for the upower daemon *)

val hibernate_allowed : t -> bool Lwt.t
val hibernate : t -> unit Lwt.t

val suspend_allowed : t -> bool Lwt.t
val suspend : t -> unit Lwt.t

val about_to_sleep : t -> unit Lwt.t

val enumerate_devices : t -> UPower_device.t list Lwt.t
val resuming : t -> unit OBus_proxy.signal
val sleeping : t -> unit OBus_proxy.signal
val changed : t -> unit OBus_proxy.signal

val device_changed : t -> UPower_device.t OBus_proxy.signal
val device_removed : t -> UPower_device.t OBus_proxy.signal
val device_added : t -> UPower_device.t OBus_proxy.signal

(** {6 Properties} *)

val lid_is_present : t -> bool Lwt.t
val set_lid_is_present : t -> bool -> unit Lwt.t

val lid_is_closed : t -> bool Lwt.t
val set_lid_is_closed : t -> bool -> unit Lwt.t

val on_low_battery : t -> bool Lwt.t
val set_on_low_battery : t -> bool -> unit Lwt.t

val on_battery : t -> bool Lwt.t
val set_on_battery : t -> bool -> unit Lwt.t

val can_hibernate : t -> bool Lwt.t
val can_suspend : t -> bool Lwt.t
val daemon_version : t -> string Lwt.t
