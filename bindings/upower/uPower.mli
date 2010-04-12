(*
 * uPower.mli
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** UPower main interface *)

include OBus_peer.Private

val daemon : unit -> t Lwt.t
  (** [daemon ()] returns the peer object for the upower daemon *)

exception General_error

(** {6 Methods} *)

val hibernate_allowed : t -> bool Lwt.t
val hibernate : t -> unit Lwt.t
val suspend_allowed : t -> bool Lwt.t
val suspend : t -> unit Lwt.t
val about_to_sleep : t -> unit Lwt.t
val enumerate_devices : t -> UPower_device.t list Lwt.t

(** {6 Signals} *)

val resuming : t -> unit OBus_signal.t
val sleeping : t -> unit OBus_signal.t
val changed : t -> unit OBus_signal.t
val device_changed : t -> UPower_device.t OBus_signal.t
val device_removed : t -> UPower_device.t OBus_signal.t
val device_added : t -> UPower_device.t OBus_signal.t

(** {6 Properties} *)

val lid_is_present : t -> bool OBus_property.r
val lid_is_closed : t -> bool OBus_property.r
val on_low_battery : t -> bool OBus_property.r
val on_battery : t -> bool OBus_property.r
val can_hibernate : t -> bool OBus_property.r
val can_suspend : t -> bool OBus_property.r
val daemon_version : t -> string OBus_property.r
