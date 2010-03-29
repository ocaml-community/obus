(*
 * NetworkManager.mli
 * ------------------
 * Copyright : (c) 2010, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type t = OBus_proxy.t
type -'a device_proxy
type connection
type active_connection
type access_point
type dhcp4_config
type ip4_config
type ip6_config
type specific_object

type ip4_address = int32
type ip6_address = char list

val ip4_to_string : ip4_address -> string

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

val string_of_state : state -> string

type device_type =
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

type unknown_kind = [ `Unknown ]
type wired_kind = [ `Wired ]
type wireless_kind = [ `Wireless ]

val string_of_device_type : device_type -> string

val t : t Lwt.t Lazy.t

val sleep : t -> bool -> unit Lwt.t
val deactivate_connection : t -> connection -> unit Lwt.t
val activate_connection : t -> string -> connection -> 'a device_proxy -> specific_object -> connection Lwt.t
val get_devices : t -> unknown_kind device_proxy list Lwt.t
val device_removed : t -> unknown_kind device_proxy OBus_proxy.signal
val device_added : t -> unknown_kind device_proxy OBus_proxy.signal
val properties_changed : t -> (string * OBus_value.single) list OBus_proxy.signal
val state : t -> < signal : state React.signal; disconnect : unit >  Lwt.t
val active_connections : t -> active_connection list Lwt.t
val wwan_hardware_enabled : t -> bool Lwt.t
val wwan_enabled : t -> bool Lwt.t
val set_wwan_enabled : t -> bool -> unit Lwt.t
val wireless_hardware_enabled : t -> bool Lwt.t
val wireless_enabled : t -> bool Lwt.t
val set_wireless_enabled : t -> bool -> unit Lwt.t

type device_with_kind =
  [ `Wired of wired_kind device_proxy
  | `Wireless of wireless_kind device_proxy
  | `Unknown of unknown_kind device_proxy ]

val get_devices_with_kind : t -> device_with_kind list Lwt.t

type device_state =
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

val string_of_device_state : device_state -> string

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

type device_capability =
  [ `Nm_supported
      (** The device is supported by NetworkManager. *)
  | `Carrier_detect
      (** The device supports carrier detection. *) ]

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

class type device =
object
  method capabilities : device_capability list Lwt.t
  method device_type : device_type Lwt.t
  method device_kind :
    [ `Unknown of device
    | `Wired of wired_device
    | `Wireless of wireless_device ] Lwt.t
  method dhcp4_config : dhcp4_config Lwt.t
  method disconnect : unit Lwt.t
  method driver : string Lwt.t
  method interface : string Lwt.t
  method ip4_address : ip4_address Lwt.t
  method ip4_config : ip4_config Lwt.t
  method ip6_config : ip6_config Lwt.t
  method managed : bool Lwt.t
  method state : device_state Lwt.t
  method state_changed : (device_state * device_state * state_reason) OBus_proxy.signal
  method udi : string Lwt.t

  method device_proxy : unknown_kind device_proxy
end

and wired_device =
object
  inherit device
  method properties_changed : (string * OBus_value.single) list OBus_proxy.signal
  method carrier : bool Lwt.t
  method speed : int Lwt.t
  method hw_address : string Lwt.t
end

and wireless_device =
object
  inherit device
  method get_access_points : access_point list Lwt.t
  method access_point_removed : access_point OBus_proxy.signal
  method access_point_added : access_point OBus_proxy.signal
  method properties_changed : (string * OBus_value.single) list OBus_proxy.signal
  method wireless_capabilities : wireless_capability list Lwt.t
  method active_access_point : access_point Lwt.t
  method bitrate : int Lwt.t
  method mode : wifi_mode Lwt.t
  method hw_address : string Lwt.t
end

val device : 'a device_proxy -> device

val wired_device : wired_kind device_proxy -> wired_device

val wireless_device : wireless_kind device_proxy -> wireless_device

type device_obj =
  [ `Wired of wired_device
  | `Wireless of wireless_device
  | `Unknown of device ]

val get_devices_obj : t -> device_obj list Lwt.t

module DeviceSet :
  sig
    include Set.S with type elt = device_obj
    val of_list : device_obj list -> t
  end

val devices : t ->
  < signal : DeviceSet.t React.signal;
    disconnect : unit > Lwt.t

type ap_security_flag =
  | Pair_wep40
  | Pair_wep104
  | Pair_tkip
  | Pair_ccmp
  | Group_wep40
  | Group_wep104
  | Group_tkip
  | Group_ccmp
  | Key_mgmt_psk
  | Key_mgmt_802

type ap_flag =
    [ `Privacy (** Access point supports privacy measures. *) ]

module Access_point : sig
  val properties_changed : access_point -> (string * OBus_value.single) list OBus_proxy.signal
  val strength : access_point -> int Lwt.t
  val max_bitrate : access_point -> int Lwt.t
  val mode : access_point -> wifi_mode Lwt.t
  val hw_address : access_point -> string Lwt.t
  val frequency : access_point -> int Lwt.t
  val ssid : access_point -> string Lwt.t
  val rsn_flags : access_point -> ap_security_flag list Lwt.t
  val wpa_flags : access_point -> ap_security_flag list Lwt.t
  val flags : access_point -> ap_flag list Lwt.t

  type info =
      {
	strength : int;
	max_bitrate : int;
	mode : wifi_mode;
	hw_address : string;
	frequency : int;
	flags : ap_flag list;
	ssid : string;
	rsn_flags : ap_security_flag list;
	wpa_flags : ap_security_flag list;
      }

  val info : access_point -> info Lwt.t

end

module Dhcp4_config : sig
  val properties_changed : dhcp4_config -> (string * OBus_value.single) list OBus_proxy.signal
  val options : dhcp4_config -> (string * OBus_value.single) list Lwt.t
end

module Ip4_config : sig
  val domains : ip4_config -> string list Lwt.t
  val wins_servers : ip4_config -> ip4_address list Lwt.t
  val nameservers : ip4_config -> ip4_address list Lwt.t

  type addr =
      { address : ip4_address;
	addr_prefix : int;
	gateway : ip4_address; }

  val addresses : ip4_config -> addr list Lwt.t

  type route =
      { route : ip4_address;
	route_prefix : int;
	next_hop : ip4_address;
	metric : int32 }

  val routes : ip4_config -> route list Lwt.t

end

module Ip6_config : sig
  type addr =
      { address : ip6_address;
	addr_prefix : int }
  type route =
      { route : ip6_address;
	route_prefix : int;
	next_hop : ip6_address;
	metric : int32 }
  val addresses : ip6_config -> addr list Lwt.t
  val nameservers : ip6_config -> ip6_address list Lwt.t
  val domains : ip6_config -> string list Lwt.t
  val routes : ip6_config -> route list Lwt.t
end

type active_connection_state =
  [ `Unknown
      (** The active connection is in an unknown state. *)
  | `Activating
      (** The connection is activating. *)
  | `Activated
      (** The connection is activated. *) ]

module Active_connection : sig
  val properties_changed : active_connection -> (string * OBus_value.single) list OBus_proxy.signal
  val vpn : active_connection -> bool Lwt.t
  val default : active_connection -> bool Lwt.t
  val state : active_connection -> active_connection_state Lwt.t
  val devices : active_connection -> unknown_kind device_proxy list Lwt.t
  val specific_object : active_connection -> specific_object Lwt.t
  val connection : active_connection -> connection Lwt.t
  val service_name : active_connection -> string Lwt.t
end

module Settings : sig
  type t = OBus_proxy.t
  val system : t Lwt.t Lazy.t
  val user : t Lwt.t Lazy.t
  val add_connection : t -> (string * (string * OBus_value.single) list) list -> unit Lwt.t
  val list_connections : t -> connection list Lwt.t
  val new_connection : t -> connection OBus_proxy.signal

  module System : sig
    val get_permissions : t -> int Lwt.t
    val save_hostname : t -> string -> unit Lwt.t
    val check_permissions : t -> unit OBus_proxy.signal
    val properties_changed : t -> (string * OBus_value.single) list OBus_proxy.signal
    val can_modify : t -> bool Lwt.t
    val hostname : t -> string Lwt.t
  end

end

module Connection : sig
  val get_settings : connection -> (string * (string * OBus_value.single) list) list Lwt.t
  val delete : connection -> unit Lwt.t
  val update : connection -> (string * (string * OBus_value.single) list) list -> unit Lwt.t
  val removed : connection -> unit OBus_proxy.signal
  val updated : connection -> (string * (string * OBus_value.single) list) list OBus_proxy.signal
  val get_secrets : connection -> string -> string list -> bool -> (string * (string * OBus_value.single) list) list Lwt.t
    (** [get_secrets c setting_name hints request_new]
	@param setting_name Name of the setting to return. 
	@param hints Lists of strings of key names in the Setting for which NM thinks a secrets may be required. 
	@param request_new Indicates whether new secrets should be requested or if the request can be fulfilled from storage. *)
end

