(*
 * uDisks.ml
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type inhibit_cookie = string
type all_spindown_timeouts_cookie = string
type inhibit_all_polling_cookie = string

type fs = {
  fs_id : string;
  fs_name : string;
  fs_supports_unix_owners : bool;
  fs_can_mount : bool;
  fs_can_create : bool;
  fs_max_label_len : int;
  fs_supports_label_rename : bool;
  fs_supports_online_label_rename : bool;
  fs_supports_fsck : bool;
  fs_supports_online_fsck : bool;
  fs_supports_resize_enlarge : bool;
  fs_supports_online_resize_enlarge : bool;
  fs_supports_resize_shrink : bool;
  fs_supports_online_resize_shrink : bool;
}

type job = {
  job_device : UDisks_device.t;
  job_in_progress : bool;
  job_is_cancellable : bool;
  job_id : string;
  job_num_tasks : int;
  job_cur_task : int;
  job_cur_task_id : string;
  job_cur_task_percentage : float;
}

(* +-----------------------------------------------------------------+
   | Exceptions                                                      |
   +-----------------------------------------------------------------+ *)

exception Busy
exception Cancelled
exception Failed
exception Filesystem_driver_missing
exception Filesystem_tools_missing
exception Inhibited
exception Invalid_option
exception Not_found
exception Not_supported
exception Permission_denied

let busy = "org.freedesktop.UDisks.Error.Busy"
let cancelled = "org.freedesktop.UDisks.Error.Cancelled"
let failed = "org.freedesktop.UDisks.Error.Failed"
let filesystem_driver_missing = "org.freedesktop.UDisks.Error.FilesystemDriverMissing"
let filesystem_tools_missing = "org.freedesktop.UDisks.Error.FilesystemToolsMissing"
let inhibited = "org.freedesktop.UDisks.Error.Inhibited"
let invalid_option = "org.freedesktop.UDisks.Error.InvalidOption"
let not_found = "org.freedesktop.UDisks.Error.NotFound"
let not_supported = "org.freedesktop.UDisks.Error.NotSupported"
let permission_denied = "org.freedesktop.UDisks.Error.PermissionDenied"

(* +-----------------------------------------------------------------+
   | D-Bus definitions                                               |
   +-----------------------------------------------------------------+ *)

include OBus_peer.Private

let daemon () =
  let%lwt bus = OBus_bus.system () in
  return (OBus_peer.make bus "org.freedesktop.UDisks")

open UDisks_interfaces.Org_freedesktop_UDisks

let proxy daemon = OBus_proxy.make daemon ["org"; "freedesktop"; "UDisks"]

let make_device context path =
  UDisks_device.of_proxy (OBus_proxy.make (OBus_context.sender context) path)
let make_adapter context path =
  UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) path)
let make_expander context path =
  UDisks_expander.of_proxy (OBus_proxy.make (OBus_context.sender context) path)
let make_port context path =
  UDisks_port.of_proxy (OBus_proxy.make (OBus_context.sender context) path)

let enumerate_adapters daemon =
  let%lwt (context, devices) = OBus_method.call_with_context m_EnumerateAdapters (proxy daemon) () in
  return (List.map (make_adapter context) devices)

let enumerate_expanders daemon =
  let%lwt (context, devices) = OBus_method.call_with_context m_EnumerateExpanders (proxy daemon) () in
  return (List.map (make_expander context) devices)

let enumerate_ports daemon =
  let%lwt (context, devices) = OBus_method.call_with_context m_EnumeratePorts (proxy daemon) () in
  return (List.map (make_port context) devices)

let enumerate_devices daemon =
  let%lwt (context, devices) = OBus_method.call_with_context m_EnumerateDevices (proxy daemon) () in
  return (List.map (make_device context) devices)

let enumerate_device_files daemon =
  OBus_method.call m_EnumerateDeviceFiles (proxy daemon) ()

let find_device_by_device_file daemon ~device_file =
  let%lwt (context, device) = OBus_method.call_with_context m_FindDeviceByDeviceFile (proxy daemon) device_file in
  return (make_device context device)

let find_device_by_major_minor daemon ~device_major ~device_minor =
  let%lwt (context, device) = OBus_method.call_with_context m_FindDeviceByMajorMinor (proxy daemon) (device_major, device_minor) in
  return (make_device context device)

let drive_inhibit_all_polling daemon ~options =
  OBus_method.call m_DriveInhibitAllPolling (proxy daemon) options

let drive_uninhibit_all_polling daemon ~cookie =
  OBus_method.call m_DriveUninhibitAllPolling (proxy daemon) cookie

let drive_set_all_spindown_timeouts daemon ~timeout_seconds ~options =
  let timeout_seconds = Int32.of_int timeout_seconds in
  OBus_method.call m_DriveSetAllSpindownTimeouts (proxy daemon) (timeout_seconds, options)

let drive_unset_all_spindown_timeouts daemon ~cookie =
  OBus_method.call m_DriveUnsetAllSpindownTimeouts (proxy daemon) cookie

let linux_lvm2_vgstart daemon ~uuid ~options =
  OBus_method.call m_LinuxLvm2VGStart (proxy daemon) (uuid, options)

let linux_lvm2_vgstop daemon ~uuid ~options =
  OBus_method.call m_LinuxLvm2VGStop (proxy daemon) (uuid, options)

let linux_lvm2_vgset_name daemon ~uuid ~name =
  OBus_method.call m_LinuxLvm2VGSetName (proxy daemon) (uuid, name)

let linux_lvm2_vgadd_pv daemon ~uuid ~physical_volume ~options =
  let physical_volume = OBus_proxy.path (UDisks_device.to_proxy physical_volume) in
  OBus_method.call m_LinuxLvm2VGAddPV (proxy daemon) (uuid, physical_volume, options)

let linux_lvm2_vgremove_pv daemon ~vg_uuid ~pv_uuid ~options =
  OBus_method.call m_LinuxLvm2VGRemovePV (proxy daemon) (vg_uuid, pv_uuid, options)

let linux_lvm2_lvset_name daemon ~group_uuid ~uuid ~name =
  OBus_method.call m_LinuxLvm2LVSetName (proxy daemon) (group_uuid, uuid, name)

let linux_lvm2_lvstart daemon ~group_uuid ~uuid ~options =
  OBus_method.call m_LinuxLvm2LVStart (proxy daemon) (group_uuid, uuid, options)

let linux_lvm2_lvremove daemon ~group_uuid ~uuid ~options =
  OBus_method.call m_LinuxLvm2LVRemove (proxy daemon) (group_uuid, uuid, options)

let linux_lvm2_lvcreate daemon ~group_uuid ~name ~size ~num_stripes ~stripe_size ~num_mirrors ~options ~fstype ~fsoptions =
  let num_stripes = Int32.of_int num_stripes in
  let num_mirrors = Int32.of_int num_mirrors in
  let%lwt (context, created_device) = OBus_method.call_with_context m_LinuxLvm2LVCreate (proxy daemon) (group_uuid, name, size, num_stripes, stripe_size, num_mirrors, options, fstype, fsoptions) in
  return (make_device context created_device)

let linux_md_start daemon ~components ~options =
  let components = List.map (fun c -> OBus_proxy.path (UDisks_device.to_proxy c)) components in
  let%lwt (context, device) = OBus_method.call_with_context m_LinuxMdStart (proxy daemon) (components, options) in
  return (make_device context device)

let linux_md_create daemon ~components ~level ~stripe_size ~name ~options =
  let components = List.map (fun c -> OBus_proxy.path (UDisks_device.to_proxy c)) components in
  let%lwt (context, device) = OBus_method.call_with_context m_LinuxMdCreate (proxy daemon) (components, level, stripe_size, name, options) in
  return (make_device context device)

let inhibit daemon =
  OBus_method.call m_Inhibit (proxy daemon) ()

let uninhibit daemon ~cookie =
  OBus_method.call m_Uninhibit (proxy daemon) cookie

let device_added daemon =
  OBus_signal.map_with_context
    make_device
    (OBus_signal.make s_DeviceAdded (proxy daemon))

let device_removed daemon =
  OBus_signal.map_with_context
    make_device
    (OBus_signal.make s_DeviceRemoved (proxy daemon))

let device_changed daemon =
  OBus_signal.map_with_context
    make_device
    (OBus_signal.make s_DeviceChanged (proxy daemon))

let device_job_changed daemon =
  OBus_signal.map_with_context
    (fun context (device, job_in_progress, job_is_cancellable, job_id, job_num_tasks, job_cur_task, job_cur_task_id, job_cur_task_percentage) -> {
       job_device = make_device context device;
       job_in_progress = job_in_progress;
       job_is_cancellable = job_is_cancellable;
       job_id = job_id;
       job_num_tasks = Int32.to_int job_num_tasks;
       job_cur_task = Int32.to_int job_cur_task;
       job_cur_task_id = job_cur_task_id;
       job_cur_task_percentage = job_cur_task_percentage;
     })
    (OBus_signal.make s_DeviceJobChanged (proxy daemon))

let adapter_added daemon =
  OBus_signal.map_with_context
    make_adapter
    (OBus_signal.make s_AdapterAdded (proxy daemon))

let adapter_removed daemon =
  OBus_signal.map_with_context
    make_adapter
    (OBus_signal.make s_AdapterRemoved (proxy daemon))

let adapter_changed daemon =
  OBus_signal.map_with_context
    make_adapter
    (OBus_signal.make s_AdapterChanged (proxy daemon))

let expander_added daemon =
  OBus_signal.map_with_context
    make_expander
    (OBus_signal.make s_ExpanderAdded (proxy daemon))

let expander_removed daemon =
  OBus_signal.map_with_context
    make_expander
    (OBus_signal.make s_ExpanderRemoved (proxy daemon))

let expander_changed daemon =
  OBus_signal.map_with_context
    make_expander
    (OBus_signal.make s_ExpanderChanged (proxy daemon))

let port_added daemon =
  OBus_signal.map_with_context
    make_port
    (OBus_signal.make s_PortAdded (proxy daemon))

let port_removed daemon =
  OBus_signal.map_with_context
    make_port
    (OBus_signal.make s_PortRemoved (proxy daemon))

let port_changed daemon =
  OBus_signal.map_with_context
    make_port
    (OBus_signal.make s_PortChanged (proxy daemon))

let daemon_version daemon =
  OBus_property.make p_DaemonVersion (proxy daemon)

let daemon_is_inhibited daemon =
  OBus_property.make p_DaemonIsInhibited (proxy daemon)

let supports_luks_devices daemon =
  OBus_property.make p_SupportsLuksDevices (proxy daemon)

let known_filesystems daemon =
  OBus_property.map_r
    (fun l ->
       List.map
         (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) -> {
            fs_id = x1;
            fs_name = x2;
            fs_supports_unix_owners = x3;
            fs_can_mount = x4;
            fs_can_create = x5;
            fs_max_label_len = Int32.to_int x6;
            fs_supports_label_rename = x7;
            fs_supports_online_label_rename = x8;
            fs_supports_fsck = x9;
            fs_supports_online_fsck = x10;
            fs_supports_resize_enlarge = x11;
            fs_supports_online_resize_enlarge = x12;
            fs_supports_resize_shrink = x13;
            fs_supports_online_resize_shrink = x14;
          })
         l)
    (OBus_property.make p_KnownFilesystems (proxy daemon))

type properties = {
  known_filesystems : fs list;
  supports_luks_devices : bool;
  daemon_is_inhibited : bool;
  daemon_version : string;
}

let properties daemon =
  OBus_property.group (proxy daemon) interface
