(*
 * nm_manager.mli
 * --------------
 * Copyright : (c) 2010, Pierre Chambart <chambart@crans.org>
 *                 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** NetworkManager main interface *)

include OBus_peer.Private

val daemon : unit -> t Lwt.t
  (** [daemon ()] returns the peer object for the network manager daemon *)

(** {6 Types} *)

(** State of the daemon *)
type state =
  [ `Unknown
      (** The NetworkManager daemon is in an unknown state. *)
  | `Asleep
      (** The NetworkManager daemon is asleep and all interfaces
	  managed by it are inactive. *)
  | `Connecting
      (** The NetworkManager daemon is connecting a device. *)
  | `Connected
      (** The NetworkManager daemon is connected. *)
  | `Disconnected
      (** The NetworkManager daemon is disconnected. *) ]

(** {6 Methods} *)

val get_devices : t -> Nm_device.t list Lwt.t
val activate_connection : t ->
  service_name : OBus_name.bus ->
  connection : Nm_settings.Connection.t ->
  device : Nm_device.t ->
  specific_object : OBus_proxy.t ->
  Nm_connection.t Lwt.t
val deactivate_connection : t -> active_connection : Nm_connection.t -> unit Lwt.t
val sleep : t -> sleep : bool -> unit Lwt.t

(** {6 Signals} *)

val state_changed : t -> state OBus_signal.t
val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t
val device_added : t -> Nm_device.t OBus_signal.t
val device_removed : t -> Nm_device.t OBus_signal.t

(** {6 Properties} *)

val wireless_enabled : t -> bool OBus_property.rw
val wireless_hardware_enabled : t -> bool OBus_property.r
val wwan_enabled : t -> bool OBus_property.rw
val wwan_hardware_enabled : t -> bool OBus_property.r
val active_connections : t -> Nm_connection.t list OBus_property.r
val state : t -> state OBus_property.r

type properties = {
  wireless_enabled : bool ;
  wireless_hardware_enabled : bool;
  wwan_enabled : bool ;
  wwan_hardware_enabled : bool;
  active_connections : Nm_connection.t list;
  state : state;
}

val properties : t -> properties OBus_property.r
