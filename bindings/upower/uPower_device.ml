(*
 * uPower_device.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

include OBus_proxy.Private

let general_error = "org.freedesktop.UPower.Device.GeneralError"

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

type state =
    [ `Unknown
    | `Charging
    | `Discharging
    | `Empty
    | `Fully_charged
    | `Pending_charge
    | `Pending_discharge ]

type technology =
    [ `Unknown
    | `Lithium_ion
    | `Lithium_polymer
    | `Lithium_iron_phosphate
    | `Lead_acid
    | `Nickel_cadmium
    | `Nickel_metal_hydride ]

open UPower_interfaces.Org_freedesktop_UPower_Device

let refresh proxy =
  OBus_method.call m_Refresh proxy ()

let changed proxy =
  OBus_signal.make s_Changed proxy

let get_history proxy ~typ ~timespan ~resolution =
  let timespan = Int32.of_int timespan in
  let resolution = Int32.of_int resolution in
  let%lwt data = OBus_method.call m_GetHistory proxy (typ, timespan, resolution) in
  let data = List.map (fun (x1, x2, x3) -> (Int32.to_int x1, x2, Int32.to_int x3)) data in
  return data

let get_statistics proxy ~typ =
  OBus_method.call m_GetStatistics proxy typ

let native_path proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_NativePath proxy

let vendor proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_Vendor proxy

let model proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_Model proxy

let serial proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_Serial proxy

let update_time proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_UpdateTime proxy

let typ proxy =
  OBus_property.map_r
    (function
       | 0l -> `Unknown
       | 1l -> `Line_power
       | 2l -> `Battery
       | 3l -> `Ups
       | 4l -> `Monitor
       | 5l -> `Mouse
       | 6l -> `Keyboard
       | 7l -> `Pda
       | 8l -> `Phone
       | n -> Printf.ksprintf failwith "invalid device type: %ld" n)
    (OBus_property.make ~monitor:UPower_monitor.monitor p_Type proxy)

let power_supply proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_PowerSupply proxy

let has_history proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_HasHistory proxy

let has_statistics proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_HasStatistics proxy

let online proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_Online proxy

let energy proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_Energy proxy

let energy_empty proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_EnergyEmpty proxy

let energy_full proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_EnergyFull proxy

let energy_full_design proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_EnergyFullDesign proxy

let energy_rate proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_EnergyRate proxy

let voltage proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_Voltage proxy

let time_to_empty proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_TimeToEmpty proxy

let time_to_full proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_TimeToFull proxy

let percentage proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_Percentage proxy

let is_present proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_IsPresent proxy

let state proxy =
  OBus_property.map_r
    (function
       | 0l -> `Unknown
       | 1l -> `Charging
       | 2l -> `Discharging
       | 3l -> `Empty
       | 4l -> `Fully_charged
       | 5l -> `Pending_charge
       | 6l -> `Pending_discharge
       | n -> Printf.ksprintf failwith "invalid device state: %ld" n)
    (OBus_property.make ~monitor:UPower_monitor.monitor p_State proxy)

let is_rechargeable proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_IsRechargeable proxy

let capacity proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_Capacity proxy

let technology proxy =
  OBus_property.map_r
    (function
       | 0l -> `Unknown
       | 1l -> `Lithium_ion
       | 2l -> `Lithium_polymer
       | 3l -> `Lithium_iron_phosphate
       | 4l -> `Lead_acid
       | 5l -> `Nickel_cadmium
       | 6l -> `Nickel_metal_hydride
       | n -> Printf.ksprintf failwith "invalid technolofy number: %ld" n)
    (OBus_property.make ~monitor:UPower_monitor.monitor p_Technology proxy)

let recall_notice proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_RecallNotice proxy

let recall_vendor proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_RecallVendor proxy

let recall_url proxy =
  OBus_property.make ~monitor:UPower_monitor.monitor p_RecallUrl proxy

let properties proxy =
  OBus_property.group ~monitor:UPower_monitor.monitor proxy interface
