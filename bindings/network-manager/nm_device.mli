(*
 * nm_device.mli
 * -------------
 * Copyright : (c) 2010, Pierre Chambart <chambart@crans.org>
 *                 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** NetworkManager devices *)

include OBus_proxy.Private

(** {6 Common interface} *)

(** {8 Types} *)

type state =
    [ `Unknown
	(** The device is in an unknown state. *)
    | `Unmanaged
	(** The device is not managed by NetworkManager. *)
    | `Unavailable
	(** The device cannot be used (carrier off, rfkill, etc)  *)
    | `Disconnected
	(** The device is not connected. *)
    | `Prepare
	(** The device is preparing to connect. *)
    | `Config
	(** The device is being configured. *)
    | `Need_auth
	(** The device is awaiting secrets necessary to continue connection. *)
    | `Ip_config
	(** The IP settings of the device are being requested and configured. *)
    | `Activated
	(** The device is active. *)
    | `Failed
	(** The device is in a failure state following an attempt to activate it. *) ]

type state_reason =
    [ `Unknown
	(** The reason for the device state change is unknown. *)
    | `None
	(** The state change is normal. *)
    | `Now_managed
	(** The device is now managed. *)
    | `Now_unmanaged
	(** The device is no longer managed. *)
    | `Config_failed
	(** The device could not be readied for configuration. *)
    | `Config_unavailable
	(** IP configuration could not be reserved (no available address, timeout, etc). *)
    | `Config_expired
	(** The IP configuration is no longer valid. *)
    | `No_secrets
	(** Secrets were required, but not provided. *)
    | `Supplicant_disconnect
	(** The 802.1X supplicant disconnected from the access point or authentication server. *)
    | `Supplicant_config_failed
	(** Configuration of the 802.1X supplicant failed. *)
    | `Supplicant_failed
	(** The 802.1X supplicant quit or failed unexpectedly. *)
    | `Supplicant_timeout
	(** The 802.1X supplicant took too long to authenticate. *)
    | `Ppp_start_failed
	(** The PPP service failed to start within the allowed time. *)
    | `Ppp_disconnect
	(** The PPP service disconnected unexpectedly. *)
    | `Ppp_failed
	(** The PPP service quit or failed unexpectedly. *)
    | `Dhcp_start_failed
	(** The DHCP service failed to start within the allowed time. *)
    | `Dhcp_error
	(** The DHCP service reported an unexpected error. *)
    | `Dhcp_failed
	(** The DHCP service quit or failed unexpectedly. *)
    | `Shared_start_failed
	(** The shared connection service failed to start. *)
    | `Shared_failed
	(** The shared connection service quit or failed unexpectedly. *)
    | `Autoip_start_failed
	(** The AutoIP service failed to start. *)
    | `Autoip_error
	(** The AutoIP service reported an unexpected error. *)
    | `Autoip_failed
	(** The AutoIP service quit or failed unexpectedly. *)
    | `Modem_busy
	(** Dialing failed because the line was busy. *)
    | `Modem_no_dial_tone
	(** Dialing failed because there was no dial tone. *)
    | `Modem_no_carrier
	(** Dialing failed because there was carrier. *)
    | `Modem_dial_timeout
	(** Dialing timed out. *)
    | `Modem_dial_failed
	(** Dialing failed. *)
    | `Modem_init_failed
	(** Modem initialization failed. *)
    | `Gsm_apn_failed
	(** Failed to select the specified GSM APN. *)
    | `Gsm_registration_not_searching
	(** Not searching for networks. *)
    | `Gsm_registration_denied
	(** Network registration was denied. *)
    | `Gsm_registration_timeout
	(** Network registration timed out. *)
    | `Gsm_registration_failed
	(** Failed to register with the requested GSM network. *)
    | `Gsm_pin_check_failed
	(** PIN check failed. *)
    | `Firmware_missing
	(** Necessary firmware for the device may be missing. *)
    | `Removed
	(** The device was removed. *)
    | `Sleeping
	(** NetworkManager went to sleep. *)
    | `Connection_removed
	(** The device's active connection was removed or disappeared. *)
    | `User_requested
	(** A user or client requested the disconnection. *)
    | `Carrier
	(** The device's carrier/link changed. *)
    | `Connection_assumed
	(** The device's existing connection was assumed. *)
    | `Supplicant_available
	(** The 802.1x supplicant is now available. *) ]

type typ =
  [ `Unknown
      (** The device type is unknown. *)
  | `Ethernet
      (** The device is wired Ethernet device. *)
  | `Wifi
      (** The device is an 802.11 WiFi device. *)
  | `Gsm
      (** The device is a GSM-based cellular WAN device. *)
  | `Cdma
      (** The device is a CDMA/IS-95-based cellular WAN device. *) ]

type capability =
  [ `Nm_supported
      (** The device is supported by NetworkManager. *)
  | `Carrier_detect
      (** The device supports carrier detection. *) ]

(** {8 Methods} *)

val disconnect : t -> unit Lwt.t

(** {8 Signals} *)

val state_changed : t -> (state * state * state_reason) OBus_signal.t

(** {8 Properties} *)

val udi : t -> string OBus_property.r
val interface : t -> string OBus_property.r
val driver : t -> string OBus_property.r
val capabilities : t -> capability list OBus_property.r
val ip4_address : t -> int32 OBus_property.r
val state : t -> state OBus_property.r
val ip4_config : t -> Nm_ip4_config.t OBus_property.r
val dhcp4_config : t -> Nm_dhcp4_config.t OBus_property.r
val ip6_config : t -> Nm_ip6_config.t OBus_property.r
val managed : t -> bool OBus_property.r
val device_type : t -> typ OBus_property.r

val properties : t -> OBus_property.group

(** {6 Specific device interfaces} *)

module Bluetooth : sig
  val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t

  val hw_address : t -> string OBus_property.r
  val name : t -> string OBus_property.r
  val bt_capabilities : t -> int OBus_property.r

  val properties : t -> OBus_property.group
end

module Cdma : sig
  val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t
end

module Gsm : sig
  val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t
end

module Olpc_mesh : sig
  val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t

  val hw_address : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t
  val companion : OBus_proxy.t -> (OBus_proxy.t, [ `readable ]) OBus_property.t
  val active_channel : OBus_proxy.t -> (int, [ `readable ]) OBus_property.t

  val properties : t -> OBus_property.group
end

module Serial : sig
  val ppp_stats : t -> (int * int) OBus_signal.t
end

module Wired : sig
  val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t

  val hw_address : t -> string OBus_property.r
  val speed : t -> int OBus_property.r
  val carrier : t -> bool OBus_property.r

  val properties : t -> OBus_property.group
end

module Wireless : sig
  type wireless_capability =
      [ `Cipher_wep40
          (** The device supports the 40-bit WEP cipher. *)
      | `Cipher_wep104
          (** The device supports the 104-bit WEP cipher. *)
      | `Cipher_tkip
          (** The device supports the TKIP cipher. *)
      | `Cipher_ccmp
          (** The device supports the CCMP cipher. *)
      | `Wpa
          (** The device supports the WPA encryption/authentication protocol. *)
      | `Rsn
          (** The device supports the RSN encryption/authentication protocol. *) ]

  type wifi_mode =
      [ `Unknown
          (** Mode is unknown. *)
      | `Adhoc
          (** Uncoordinated network without central infrastructure. *)
      | `Infra
          (** Coordinated network with one or more central controllers. *) ]

  val get_access_points : t -> Nm_access_point.t list Lwt.t

  val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t
  val access_point_added : t -> Nm_access_point.t OBus_signal.t
  val access_point_removed : t -> Nm_access_point.t OBus_signal.t

  val hw_address : t -> string OBus_property.r
  val mode : t -> int OBus_property.r
  val bitrate : t -> int OBus_property.r
  val active_access_point : t -> OBus_proxy.t OBus_property.r
  val wireless_capabilities : t -> int OBus_property.r

  val properties : t -> OBus_property.group
end
