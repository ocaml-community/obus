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

exception General_error
let () = OBus_error.register ~name:"org.freedesktop.UPower.Device.GeneralError" ~exn:General_error

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

let notify_mode = OBus_property.notify_global "Changed"

let refresh proxy =
  OBus_method.call m_Refresh proxy ()

let changed proxy =
  OBus_signal.connect s_Changed proxy

let get_history proxy ~typ ~timespan ~resolution =
  let timespan = Int32.of_int timespan in
  let resolution = Int32.of_int resolution in
  lwt data = OBus_method.call m_GetHistory proxy (typ, timespan, resolution) in
  let data = List.map (fun (x1, x2, x3) -> (Int32.to_int x1, x2, Int32.to_int x3)) data in
  return data

let get_statistics proxy ~typ =
  OBus_method.call m_GetStatistics proxy typ

let native_path proxy =
  OBus_property.make p_NativePath ~notify_mode proxy

let vendor proxy =
  OBus_property.make p_Vendor ~notify_mode proxy

let model proxy =
  OBus_property.make p_Model ~notify_mode proxy

let serial proxy =
  OBus_property.make p_Serial ~notify_mode proxy

let update_time proxy =
  OBus_property.make p_UpdateTime ~notify_mode proxy

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
    (OBus_property.make p_Type ~notify_mode proxy)

let power_supply proxy =
  OBus_property.make p_PowerSupply ~notify_mode proxy

let has_history proxy =
  OBus_property.make p_HasHistory ~notify_mode proxy

let has_statistics proxy =
  OBus_property.make p_HasStatistics ~notify_mode proxy

let online proxy =
  OBus_property.make p_Online ~notify_mode proxy

let energy proxy =
  OBus_property.make p_Energy ~notify_mode proxy

let energy_empty proxy =
  OBus_property.make p_EnergyEmpty ~notify_mode proxy

let energy_full proxy =
  OBus_property.make p_EnergyFull ~notify_mode proxy

let energy_full_design proxy =
  OBus_property.make p_EnergyFullDesign ~notify_mode proxy

let energy_rate proxy =
  OBus_property.make p_EnergyRate ~notify_mode proxy

let voltage proxy =
  OBus_property.make p_Voltage ~notify_mode proxy

let time_to_empty proxy =
  OBus_property.make p_TimeToEmpty ~notify_mode proxy

let time_to_full proxy =
  OBus_property.make p_TimeToFull ~notify_mode proxy

let percentage proxy =
  OBus_property.make p_Percentage ~notify_mode proxy

let is_present proxy =
  OBus_property.make p_IsPresent ~notify_mode proxy

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
    (OBus_property.make p_State ~notify_mode proxy)

let is_rechargeable proxy =
  OBus_property.make p_IsRechargeable ~notify_mode proxy

let capacity proxy =
  OBus_property.make p_Capacity ~notify_mode proxy

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
    (OBus_property.make p_Technology ~notify_mode proxy)

let recall_notice proxy =
  OBus_property.make p_RecallNotice ~notify_mode proxy

let recall_vendor proxy =
  OBus_property.make p_RecallVendor ~notify_mode proxy

let recall_url proxy =
  OBus_property.make p_RecallUrl ~notify_mode proxy

type properties = {
  recall_url : string;
  recall_vendor : string;
  recall_notice : bool;
  technology : technology;
  capacity : float;
  is_rechargeable : bool;
  state : state;
  is_present : bool;
  percentage : float;
  time_to_full : int64;
  time_to_empty : int64;
  voltage : float;
  energy_rate : float;
  energy_full_design : float;
  energy_full : float;
  energy_empty : float;
  energy : float;
  online : bool;
  has_statistics : bool;
  has_history : bool;
  power_supply : bool;
  typ : typ;
  update_time : int64;
  serial : string;
  model : string;
  vendor : string;
  native_path : string;
}

let properties proxy =
  OBus_property.map_r_with_context
    (fun context properties ->
       let find f = OBus_property.find (f proxy) context properties in
       {
         recall_url = find recall_url;
         recall_vendor = find recall_vendor;
         recall_notice = find recall_notice;
         technology = find technology;
         capacity = find capacity;
         is_rechargeable = find is_rechargeable;
         state = find state;
         is_present = find is_present;
         percentage = find percentage;
         time_to_full = find time_to_full;
         time_to_empty = find time_to_empty;
         voltage = find voltage;
         energy_rate = find energy_rate;
         energy_full_design = find energy_full_design;
         energy_full = find energy_full;
         energy_empty = find energy_empty;
         energy = find energy;
         online = find online;
         has_statistics = find has_statistics;
         has_history = find has_history;
         power_supply = find power_supply;
         typ = find typ;
         update_time = find update_time;
         serial = find serial;
         model = find model;
         vendor = find vendor;
         native_path = find native_path;
       })
    (OBus_property.make_group proxy ~notify_mode interface)
