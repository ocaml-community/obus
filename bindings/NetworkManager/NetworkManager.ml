open Lwt
open OBus_pervasives

let section = Lwt_log.Section.make "NetworkManager"

type t = OBus_proxy.t with obus
type 'a device_proxy = OBus_proxy.t
type connection = OBus_proxy.t
type active_connection = OBus_proxy.t
type access_point = OBus_proxy.t
type dhcp4_config = OBus_proxy.t
type ip4_config = OBus_proxy.t
type ip6_config = OBus_proxy.t
type specific_object = OBus_proxy.t

type ip4_address = int32
type ip6_address = char list

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

let state_of_int = function
  | 0 -> `Unknown
  | 1 -> `Asleep
  | 2 -> `Connecting
  | 3 -> `Connected
  | 4 -> `Disconnected
  | i ->
      ignore (Lwt_log.warning_f ~section "state_of_int: unknown state: %i" i);
      `Unknown

(*let state_to_int = function
  | `Unknown -> 0
  | `Asleep -> 1
  | `Connecting -> 2
  | `Connected -> 3
  | `Disconnected -> 4*)

let string_of_state = function
  | `Unknown -> "unknown"
  | `Asleep -> "asleep"
  | `Connecting -> "connecting"
  | `Connected -> "connected"
  | `Disconnected -> "disconnected"

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

let string_of_device_type = function
  | `Unknown -> "unknown"
  | `Ethernet -> "ethernet"
  | `Wifi -> "wifi"
  | `Gsm -> "gsm"
  | `Cdma -> "cdma"

let device_type_of_int = function
  | 0 -> `Unknown
  | 1 -> `Ethernet
  | 2 -> `Wifi
  | 3 -> `Gsm
  | 4 -> `Cdma
  | i ->
      ignore (Lwt_log.warning_f ~section "device_type_of_int: unknown type: %i" i);
      `Unknown

type unknown_kind = [ `Unknown ]
type wired_kind = [ `Wired ]
type wireless_kind = [ `Wireless ]

let kind_of_type = function
  | `Unknown -> `Unknown
  | `Ethernet -> `Wired
  | `Wifi
  | `Gsm
  | `Cdma -> `Wireless

let t =
  lazy(lwt bus = OBus_bus.system () in
       return (OBus_proxy.make
                 (OBus_peer.make bus "org.freedesktop.NetworkManager")
                 [ "org"; "freedesktop"; "NetworkManager" ]))

let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager"
OP_method Sleep : bool -> unit
OP_method DeactivateConnection : OBus_proxy.t -> unit
OP_method ActivateConnection : string -> OBus_proxy.t -> OBus_proxy.t -> OBus_proxy.t -> OBus_proxy.t
OP_method GetDevices : OBus_proxy.t list
OP_signal DeviceRemoved : OBus_proxy.t
OP_signal DeviceAdded : OBus_proxy.t
OP_signal PropertiesChanged : (string, variant) dict
OP_property_r ActiveConnections : OBus_proxy.t list
OP_property_r WwanHardwareEnabled : bool
OP_property_rw WwanEnabled : bool
OP_property_r WirelessHardwareEnabled : bool
OP_property_rw WirelessEnabled : bool

module Internal =
struct
  OP_property_r State : uint
  OP_signal StateChanged : uint
end

let state t =
  let e = Internal.state_changed t in
  let event = e#event in
  lwt state = Internal.state t in
  let signal = React.S.hold state event in
  let s = React.S.map state_of_int signal in
  return
    ( object
      method signal = s
      method disconnect = e#disconnect
      end )

let ip4_to_string v =
  let b1 = Int32.logand 0xFFl v
  and b2 = Int32.logand 0xFFl (Int32.shift_right v 8)
  and b3 = Int32.logand 0xFFl (Int32.shift_right v 16)
  and b4 = Int32.logand 0xFFl (Int32.shift_right v 24) in
  (Printf.sprintf "%ld.%ld.%ld.%ld" b1 b2 b3 b4)

(*
let inet4_to_int32 v =
  let f b4 b3 b2 b1 =
    Int32.logor
      (Int32.logor
	 (Int32.shift_left b1 24)
	 (Int32.shift_left b2 16))
      (Int32.logor
	 (Int32.shift_left b3 8)
	 b4)
  in
  let s = Unix.string_of_inet_addr v in
  Scanf.sscanf s "%li.%li.%li.%li" f
*)

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

let device_state_of_int = function
  | 0 -> `Unknown
  | 1 -> `Unmanaged
  | 2 -> `Unavailable
  | 3 -> `Disconnected
  | 4 -> `Prepare
  | 5 -> `Config
  | 6 -> `Need_auth
  | 7 -> `Ip_config
  | 8 -> `Activated
  | 9 -> `Failed
  | i ->
      ignore (Lwt_log.warning_f ~section "device_state_of_int: unknown device_state: %i" i);
      `Unknown

let string_of_device_state = function
  | `Unknown -> "unknown"
  | `Unmanaged -> "unmanaged"
  | `Unavailable -> "unavailable"
  | `Disconnected -> "disconnected"
  | `Prepare -> "prepare"
  | `Config -> "config"
  | `Need_auth -> "need auth"
  | `Ip_config -> "ip config"
  | `Activated -> "activated"
  | `Failed -> "failed"

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

let state_reason_of_int = function
  |  0 -> `Unknown
  |  1 -> `None
  |  2 -> `Now_managed
  |  3 -> `Now_unmanaged
  |  4 -> `Config_failed
  |  5 -> `Config_unavailable
  |  6 -> `Config_expired
  |  7 -> `No_secrets
  |  8 -> `Supplicant_disconnect
  |  9 -> `Supplicant_config_failed
  | 10 -> `Supplicant_failed
  | 11 -> `Supplicant_timeout
  | 12 -> `Ppp_start_failed
  | 13 -> `Ppp_disconnect
  | 14 -> `Ppp_failed
  | 15 -> `Dhcp_start_failed
  | 16 -> `Dhcp_error
  | 17 -> `Dhcp_failed
  | 18 -> `Shared_start_failed
  | 19 -> `Shared_failed
  | 20 -> `Autoip_start_failed
  | 21 -> `Autoip_error
  | 22 -> `Autoip_failed
  | 23 -> `Modem_busy
  | 24 -> `Modem_no_dial_tone
  | 25 -> `Modem_no_carrier
  | 26 -> `Modem_dial_timeout
  | 27 -> `Modem_dial_failed
  | 28 -> `Modem_init_failed
  | 29 -> `Gsm_apn_failed
  | 30 -> `Gsm_registration_not_searching
  | 31 -> `Gsm_registration_denied
  | 32 -> `Gsm_registration_timeout
  | 33 -> `Gsm_registration_failed
  | 34 -> `Gsm_pin_check_failed
  | 35 -> `Firmware_missing
  | 36 -> `Removed
  | 37 -> `Sleeping
  | 38 -> `Connection_removed
  | 39 -> `User_requested
  | 40 -> `Carrier
  | 41 -> `Connection_assumed
  | 42 -> `Supplicant_available
  | i ->
      ignore (Lwt_log.warning_f ~section "state_reason_of_int: unknown state_reason: %i" i);
      `Unknown

type device_capability =
  [ `Nm_supported
      (** The device is supported by NetworkManager. *)
  | `Carrier_detect
      (** The device supports carrier detection. *) ]

let device_capabilities_flag i =
  let l = [] in
  let l = if i land 0x1 = 0x1
  then `Nm_supported::l
  else l
  in
  let l = if i land 0x2 = 0x2
  then `Carrier_detect::l
  else l
  in
  l

module Device = struct
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager.Device"
  OP_method Disconnect : unit
  OP_property_r Managed : bool
  OP_property_r Ip6Config : OBus_proxy.t
  OP_property_r Dhcp4Config : OBus_proxy.t
  OP_property_r Ip4Config : OBus_proxy.t
  OP_property_r Driver : string
  OP_property_r Interface : string
  OP_property_r Udi : string

  type dev_cap = device_capability list

  let obus_dev_cap =
    OBus_type.map obus_uint
      device_capabilities_flag
      (fun _ -> assert false) (* device capabilities are not sent *)

  OP_property_r Capabilities : dev_cap

  let obus_device_type =
    OBus_type.map obus_uint
      device_type_of_int
      (fun _ -> assert false) (* device_type are not sent *)

  OP_property_r DeviceType : device_type

  let obus_device_state =
    OBus_type.map obus_uint
      device_state_of_int
      (fun _ -> assert false) (* device_state are not sent *)

  OP_property_r State : device_state

  let obus_state_reason =
    OBus_type.map obus_uint
      state_reason_of_int
      (fun _ -> assert false) (* state_reason are not sent *)

  OP_signal StateChanged : (device_state * device_state * state_reason)

  OP_property_r Ip4Address : uint32

(*
  let set_ip4_address device addr =
    match Unix.domain_of_sockaddr (Unix.ADDR_INET (addr,0)) with
      | Unix.PF_INET ->
	  let addr = inet4_to_int32 addr in
	  Internal.set_ip4_address device addr
      | _ ->
	  fail (Invalid_argument "NetworkManager: set_ip4_address: not an ipv4 address")
*)

end

module Wired = struct
  type t = OBus_proxy.t with obus
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager.Device.Wired"
  OP_signal PropertiesChanged : (string, variant) dict
  OP_property_r Carrier : bool
  OP_property_r Speed : uint
  OP_property_r HwAddress : string
end

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

let wireless_capabilities_flag i =
  let l = [] in
  let l = if i land 0x1 = 0x1
  then `Cipher_wep40::l
  else l
  in
  let l = if i land 0x2 = 0x2
  then `Cipher_wep104::l
  else l
  in
  let l = if i land 0x4 = 0x4
  then `Cipher_tkip::l
  else l
  in
  let l = if i land 0x8 = 0x8
  then `Cipher_ccmp::l
  else l
  in
  let l = if i land 0x10 = 0x10
  then `Wpa::l
  else l
  in
  let l = if i land 0x20 = 0x20
  then `Rsn::l
  else l
  in
  l

type wifi_mode =
  [ `Unknown
      (** Mode is unknown. *)
  | `Adhoc
      (** Uncoordinated network without central infrastructure. *)
  | `Infra
      (** Coordinated network with one or more central controllers. *) ]

let wifi_mode_of_int = function
  | 0 -> `Unknown
  | 1 -> `Adhoc
  | 2 -> `Infra
  | i ->
      ignore (Lwt_log.warning_f ~section "wifi_mode_of_int: unknown mode: %i" i);
      `Unknown

let obus_wifi_mode =
  OBus_type.map obus_uint
    wifi_mode_of_int
    (fun _ -> assert false)

module Wireless = struct
  type t = OBus_proxy.t with obus
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager.Device.Wireless"
  OP_method GetAccessPoints : OBus_proxy.t list
  OP_signal AccessPointRemoved : OBus_proxy.t
  OP_signal AccessPointAdded : OBus_proxy.t
  OP_signal PropertiesChanged : (string, variant) dict
  OP_property_r ActiveAccessPoint : OBus_proxy.t
  OP_property_r Bitrate : uint
  OP_property_r HwAddress : string

  type wi_cap = wireless_capability list

  let obus_wi_cap =
    OBus_type.map obus_uint
      wireless_capabilities_flag
      (fun _ -> assert false) (* wireless capabilities are not sent *)

  OP_property_r WirelessCapabilities : wi_cap

  OP_property_r Mode : wifi_mode


end

type device_with_kind =
  [ `Wired of wired_kind device_proxy
  | `Wireless of wireless_kind device_proxy
  | `Unknown of unknown_kind device_proxy ]

let get_devices_with_kind t =
  lwt devices = get_devices t in
  let f device =
    lwt type_ = Device.device_type device in
    return (
      match kind_of_type type_ with
	| `Wired -> `Wired device
	| `Wireless -> `Wireless device
	| `Unknown -> `Unknown device )
  in
  Lwt_list.map_p f devices

class type device =
object
  method capabilities : device_capability list Lwt.t
  method device_type : device_type Lwt.t
  method device_kind :
    [ `Unknown of device
    | `Wired of wired_device
    | `Wireless of wireless_device ] Lwt.t
  method dhcp4_config : OBus_proxy.t Lwt.t
  method disconnect : unit Lwt.t
  method driver : string Lwt.t
  method interface : string Lwt.t
  method ip4_address : ip4_address Lwt.t
  method ip4_config : OBus_proxy.t Lwt.t
  method ip6_config : OBus_proxy.t Lwt.t
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

class virtual device_skel =
object (self:'self)
  val virtual proxy : OBus_proxy.t
  method disconnect = Device.disconnect proxy
  method state_changed = Device.state_changed proxy
  method device_type : device_type Lwt.t = Device.device_type proxy
  method device_kind :
    [ `Unknown of device
    | `Wired of wired_device
    | `Wireless of wireless_device ] Lwt.t =
    lwt device_type = Device.device_type proxy in
    return (
      match device_type with
	| `Unknown -> `Unknown (self:>device)
	| `Ethernet -> `Wired (new wired_device_concrete proxy)
	| `Wifi
	| `Gsm
	| `Cdma -> `Wireless (new wireless_device_concrete proxy))

  method managed = Device.managed proxy
  method ip6_config = Device.ip6_config proxy
  method dhcp4_config = Device.dhcp4_config proxy
  method ip4_config = Device.ip4_config proxy
  method state = Device.state proxy
  method ip4_address = Device.ip4_address proxy
  method capabilities = Device.capabilities proxy
  method driver = Device.driver proxy
  method interface = Device.interface proxy
  method udi = Device.udi proxy

  method device_proxy = proxy
end

and virtual wired_device_skel =
object (self)
  inherit device_skel
  method properties_changed = Wired.properties_changed proxy
  method carrier = Wired.carrier proxy
  method speed = Wired.speed proxy
  method hw_address = Wired.hw_address proxy
  method device_kind =
    return (`Wired (self:>wired_device_skel))
end

and virtual wireless_device_skel =
object (self)
  inherit device_skel
  method get_access_points = Wireless.get_access_points proxy
  method access_point_removed = Wireless.access_point_removed proxy
  method access_point_added = Wireless.access_point_added proxy
  method properties_changed = Wireless.properties_changed proxy
  method wireless_capabilities = Wireless.wireless_capabilities proxy
  method active_access_point = Wireless.active_access_point proxy
  method bitrate = Wireless.bitrate proxy
  method mode = Wireless.mode proxy
  method hw_address = Wireless.hw_address proxy
  method device_kind =
    return (`Wireless (self:>wireless_device_skel))
end

and device_concrete device =
object
  val proxy = device
  inherit device_skel
end

and wired_device_concrete device =
object
  val proxy = device
  inherit wired_device_skel
end

and wireless_device_concrete device =
object
  val proxy = device
  inherit wireless_device_skel
end

let device device = new device_concrete device

let wired_device device = new wired_device_concrete device

let wireless_device device = new wireless_device_concrete device

type device_obj =
  [ `Wired of wired_device_skel
  | `Wireless of wireless_device_skel
  | `Unknown of device_skel ]

let get_device_obj dev =
  lwt type_ = Device.device_type dev in
  return (
    match kind_of_type type_ with
      | `Wired -> `Wired (wired_device dev)
      | `Wireless -> `Wireless (wireless_device dev)
      | `Unknown -> `Unknown (device dev) )

let get_devices_obj t =
  lwt devices = get_devices t in
  Lwt_list.map_p get_device_obj devices

let dev_of_obj : device_obj -> 'a = function
  | `Wired d -> d#device_proxy
  | `Wireless d -> d#device_proxy
  | `Unknown d -> d#device_proxy

module DeviceSet =
struct

  include Set.Make(
    struct
      type t = device_obj
      let compare dev1 dev2 = compare (dev_of_obj dev1) (dev_of_obj dev2)
    end)

  let of_list l =
    let f s e = add e s in
    List.fold_left f empty l

end

let devices t =
  lwt device_list = get_devices_obj t in
  let init_set = DeviceSet.of_list device_list in
  let add_signal = device_added t in
  let add_event = React.E.map (fun d -> ()) add_signal#event in
  let remove_signal = device_removed t in
  let remove_event = React.E.map (fun d -> ()) remove_signal#event in
  let changes = React.E.select [add_event;remove_event] in
  let f () =
    lwt devs = get_devices_obj t in
    return (DeviceSet.of_list devs)
  in
  let changes' = Lwt_event.map_s f changes in
  let s = React.S.hold ~eq:DeviceSet.equal init_set changes' in
  return (
    object
      method signal = s
      method disconnect =
	add_signal#disconnect;
	remove_signal#disconnect
    end )

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

let ap_security_flag i =
  let l = [] in
  let l = if i land 0x1 = 0x1
  then Pair_wep40::l
  else l
  in
  let l = if i land 0x2 = 0x2
  then Pair_wep104::l
  else l
  in
  let l = if i land 0x4 = 0x4
  then Pair_tkip::l
  else l
  in
  let l = if i land 0x8 = 0x8
  then Pair_ccmp::l
  else l
  in
  let l = if i land 0x10 = 0x10
  then Group_wep40::l
  else l
  in
  let l = if i land 0x20 = 0x20
  then Group_wep104::l
  else l
  in
  let l = if i land 0x40 = 0x40
  then Group_tkip::l
  else l
  in
  let l = if i land 0x80 = 0x80
  then Group_ccmp::l
  else l
  in
  let l = if i land 0x100 = 0x100
  then Key_mgmt_psk::l
  else l
  in
  let l = if i land 0x200 = 0x200
  then Key_mgmt_802::l
  else l
  in
  l

type ap_flag =
    [ `Privacy (** Access point supports privacy measures. *) ]

let ap_flag i =
  let l = [] in
  let l = if i land 0x1 = 0x1
  then `Privacy::l
  else l
  in
  l

let string_of_char_list l =
  let length = List.length l in
  let s = String.create length in
  let p = ref 0 in
  let f c = s.[!p] <- c; incr p in
  List.iter f l;
  s

module Access_point = struct
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

  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager.AccessPoint"
  OP_signal PropertiesChanged : (string, variant) dict
  OP_property_r MaxBitrate : uint
  OP_property_r Mode : wifi_mode
  OP_property_r HwAddress : string
  OP_property_r Frequency : uint

  type ap_fl = ap_flag list

  let obus_ap_fl =
    OBus_type.map obus_uint
      ap_flag
      ( fun _ -> assert false )

  OP_property_r Flags : ap_fl

  module Internal =
  struct
    OP_property_r RsnFlags : uint
    OP_property_r WpaFlags : uint
    OP_property_r Ssid : char list
    OP_property_r Strength : char
  end
  let rsn_flags ap =
    lwt i = Internal.rsn_flags ap in
    return (ap_security_flag i)
  let wpa_flags ap =
    lwt i = Internal.wpa_flags ap in
    return (ap_security_flag i)
  let ssid ap =
    lwt l = Internal.ssid ap in
    return (string_of_char_list l)
  let strength ap =
    lwt c = Internal.strength ap in
    return (int_of_char c)

  let info ap =
    let t_strength = strength ap in
    let t_max_bitrate = max_bitrate ap in
    let t_mode = mode ap in
    let t_rsn_flags = rsn_flags ap in
    let t_wpa_flags = wpa_flags ap in
    let t_ssid = ssid ap in
    let t_frequency = frequency ap in
    let t_flags = flags ap in
    let t_hw_address = hw_address ap in
    lwt strength = t_strength in
    lwt max_bitrate = t_max_bitrate in
    lwt mode = t_mode in
    lwt rsn_flags = t_rsn_flags in
    lwt wpa_flags = t_wpa_flags in
    lwt ssid = t_ssid in
    lwt frequency = t_frequency in
    lwt flags = t_flags in
    lwt hw_address = t_hw_address in
    return {
      strength = strength;
      max_bitrate = max_bitrate;
      mode = mode;
      rsn_flags = rsn_flags;
      wpa_flags = wpa_flags;
      ssid = ssid;
      frequency = frequency;
      flags = flags;
      hw_address = hw_address;
    }

end

module Dhcp4_config = struct
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager.DHCP4Config"
  OP_signal PropertiesChanged : (string, variant) dict
  OP_property_r Options : (string, variant) dict
end

module Ip4_config = struct
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager.IP4Config"
  OP_property_r Domains : string list
  OP_property_r WinsServers : uint32 list
  OP_property_r Nameservers : uint32 list

  type addr =
      { address : int32;
	addr_prefix : int;
	gateway : int32; }

  let conv_addrs addrs =
    let f = function
      | [a;b;c] ->
	  { address = a;
	    addr_prefix = Int32.to_int b;
	    gateway = c }
      | _ -> failwith "malformed address triplet"
    in
    List.map f addrs

  let obus_addrs =
    OBus_type.map
      (obus_list (obus_list obus_uint32))
      conv_addrs
      ( fun _ -> assert false )

  OP_property_r Addresses : addrs

  type route =
      { route : int32;
	route_prefix : int;
	next_hop : int32;
	metric : int32 }

  let conv_routes routes =
    let f = function
      | [a;b;c;d] ->
	  { route = a;
	    route_prefix = Int32.to_int b;
	    next_hop = c;
	    metric = d }
      | _ -> failwith "malformed route quadruplet"
    in
    List.map f routes

  let obus_routes =
    OBus_type.map
      (obus_list (obus_list obus_uint32))
      conv_routes
      ( fun _ -> assert false )

  OP_property_r Routes : routes

end

type active_connection_state =
  [ `Unknown
      (** The active connection is in an unknown state. *)
  | `Activating
      (** The connection is activating. *)
  | `Activated
      (** The connection is activated. *) ]

let active_connection_state_of_int = function
  | 0 -> `Unknown
  | 1 -> `Activating
  | 2 -> `Activated
  | i ->
      ignore (Lwt_log.warning_f ~section "active_connection_state_of_int: unknown state: %i" i);
      `Unknown

module Active_connection = struct
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager.Connection.Active"
  OP_signal PropertiesChanged : (string, variant) dict
  OP_property_r Vpn : bool
  OP_property_r Default : bool
  OP_property_r Devices : OBus_proxy.t list
  OP_property_r SpecificObject : OBus_proxy.t
  OP_property_r Connection : OBus_proxy.t
  OP_property_r ServiceName : string

  let obus_active_connection_state =
    OBus_type.map obus_uint
      active_connection_state_of_int
      ( fun _ -> assert false )

  OP_property_r State : active_connection_state

end

module Ip6_config = struct
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManager.IP6Config"
  OP_property_r Nameservers : char list list
  OP_property_r Domains : string list

  type addr =
      { address : ip6_address;
	addr_prefix : int }
  type route =
      { route : ip6_address;
	route_prefix : int;
	next_hop : ip6_address;
	metric : int32 }

  let conv_addrs l =
    List.map ( fun ( a,p ) -> {address=a;addr_prefix=p} ) l

  let conv_routes l =
    List.map ( fun ( r,p,h,m ) -> {route=r;route_prefix=p;next_hop=h;metric=m} ) l

  let obus_addrs =
    OBus_type.map (obus_list (obus_structure (OBus_type.tuple2 (obus_list obus_char) obus_uint)))
      conv_addrs
      ( fun _ -> assert false )

  let obus_routes =
    OBus_type.map (obus_list (obus_structure 
				(OBus_type.tuple4
				   (obus_list obus_char)
				   obus_uint
				   (obus_list obus_char)
				   obus_uint32
				)))
      conv_routes
      ( fun _ -> assert false )

  OP_property_r Addresses : addrs

  OP_property_r Routes : routes

end


module Settings = struct
  type t = OBus_proxy.t with obus
  let system =
    lazy(lwt bus = OBus_bus.system () in
    return (OBus_proxy.make
              (OBus_peer.make bus "org.freedesktop.NetworkManagerSystemSettings")
              [ "org"; "freedesktop"; "NetworkManagerSettings" ]))
  let user =
    lazy(lwt bus = OBus_bus.session () in
    return (OBus_proxy.make
              (OBus_peer.make bus "org.freedesktop.NetworkManagerUserSettings")
              [ "org"; "freedesktop"; "NetworkManagerSettings" ]))
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManagerSettings"
  OP_method AddConnection : (string, (string, variant) dict) dict -> unit
  OP_method ListConnections : OBus_proxy.t list
  OP_signal NewConnection : OBus_proxy.t

  module System = struct
    let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManagerSettings.System"
    OP_method GetPermissions : uint
    OP_method SaveHostname : string -> unit
    OP_signal CheckPermissions : unit
    OP_signal PropertiesChanged : (string, variant) dict
    OP_property_r CanModify : bool
    OP_property_r Hostname : string
  end
end

module Connection = struct
  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManagerSettings.Connection"
  OP_method GetSettings : (string, (string, variant) dict) dict
  OP_method Delete : unit
  OP_method Update : (string, (string, variant) dict) dict -> unit
  OP_signal Removed : unit
  OP_signal Updated : (string, (string, variant) dict) dict

  let op_interface = OBus_proxy.make_interface "org.freedesktop.NetworkManagerSettings.Connection.Secrets"
  OP_method GetSecrets : string -> string list -> bool -> (string, (string, variant) dict) dict
end
