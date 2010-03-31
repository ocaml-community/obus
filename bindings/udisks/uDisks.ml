(*
 * uDisks.ml
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_pervasives

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type inhibit_cookie = string with obus
type all_spindown_timeouts_cookie = string with obus
type inhibit_all_polling_cookie = string with obus

type fs = {
  fs_id : string;
  fs_name : string;
  fs_supports_unix_owners : bool;
  fs_can_mount : bool;
  fs_can_create : bool;
  fs_max_label_len : uint;
  fs_supports_label_rename : bool;
  fs_supports_online_label_rename : bool;
  fs_supports_fsck : bool;
  fs_supports_online_fsck : bool;
  fs_supports_resize_enlarge : bool;
  fs_supports_online_resize_enlarge : bool;
  fs_supports_resize_shrink : bool;
  fs_supports_online_resize_shrink : bool;
} with obus

(* +-----------------------------------------------------------------+
   | Exceptions                                                      |
   +-----------------------------------------------------------------+ *)

exception Busy of string
 with obus("org.freedesktop.UDisks.Error.Busy")

exception Cancelled of string
 with obus("org.freedesktop.UDisks.Error.Cancelled")

exception Failed of string
 with obus("org.freedesktop.UDisks.Error.Failed")

exception Filesystem_driver_missing of string
 with obus("org.freedesktop.UDisks.Error.FilesystemDriverMissing")

exception Filesystem_tools_missing of string
 with obus("org.freedesktop.UDisks.Error.FilesystemToolsMissing")

exception Inhibited of string
 with obus("org.freedesktop.UDisks.Error.Inhibited")

exception Invalid_option of string
 with obus("org.freedesktop.UDisks.Error.InvalidOption")

exception Not_found of string
 with obus("org.freedesktop.UDisks.Error.NotFound")

exception Not_supported of string
 with obus("org.freedesktop.UDisks.Error.NotSupported")

exception Permission_denied of string
 with obus("org.freedesktop.UDisks.Error.PermissionDenied")

(* +-----------------------------------------------------------------+
   | D-Bus definitions                                               |
   +-----------------------------------------------------------------+ *)

include OBus_peer.Private

let daemon () =
  lwt bus = OBus_bus.system () in
  Lwt.return (OBus_peer.make bus "org.freedesktop.UDisks")

module Proxy = OBus_proxy.Make(struct
                                 type proxy = t
                                 let cast peer = OBus_proxy.make peer ["org"; "freedesktop"; "UDisks"]
                                 let make = OBus_proxy.peer
                               end)

let op_interface = Proxy.make_interface "org.freedesktop.UDisks"

OP_method Uninhibit : cookie : inhibit_cookie -> unit
OP_method Inhibit : inhibit_cookie

OP_method LinuxMdCreate : components : UDisks_device.t list -> level : string -> stripe_size : uint64 -> name : string -> options : string list -> UDisks_device.t
OP_method LinuxMdStart : components : UDisks_device.t list -> options : string list -> UDisks_device.t

OP_method LinuxLvm2LVCreate : group_uuid : string -> name : string -> size : uint64 -> num_stripes : uint -> stripe_size : uint64 -> num_mirrors : uint -> options : string list -> fstype : string -> fsoptions : string list -> UDisks_device.t
OP_method LinuxLvm2LVRemove : group_uuid : string -> uuid : string -> options : string list -> unit
OP_method LinuxLvm2LVStart : group_uuid : string -> uuid : string -> options : string list -> unit
OP_method LinuxLvm2LVSetName : group_uuid : string -> uuid : string -> name : string -> unit
OP_method LinuxLvm2VGRemovePV : vg_uuid : string -> pv_uuid : string -> options : string list -> unit
OP_method LinuxLvm2VGAddPV : uuid : string -> physical_volume : UDisks_device.t -> options : string list -> unit
OP_method LinuxLvm2VGSetName : uuid : string -> name : string -> unit
OP_method LinuxLvm2VGStop : uuid : string -> options : string list -> unit
OP_method LinuxLvm2VGStart : uuid : string -> options : string list -> unit

OP_method DriveSetAllSpindownTimeouts : timeout_seconds : int -> options : string list -> all_spindown_timeouts_cookie
OP_method DriveUnsetAllSpindownTimeouts : cookie : all_spindown_timeouts_cookie -> unit

OP_method DriveInhibitAllPolling : options : string list -> inhibit_all_polling_cookie
OP_method DriveUninhibitAllPolling : cookie : inhibit_all_polling_cookie -> unit

OP_method FindDeviceByDeviceFile : device_file : string -> UDisks_device.t
OP_method FindDeviceByMajorMinor : device_major : int64 -> device_minor : int64 -> UDisks_device.t

OP_method EnumerateDeviceFiles : string list
OP_method EnumerateDevices : UDisks_device.t list
OP_method EnumeratePorts : UDisks_port.t list
OP_method EnumerateExpanders : UDisks_expander.t list
OP_method EnumerateAdapters : UDisks_adapter.t list

OP_signal PortChanged : UDisks_port.t
OP_signal PortRemoved : UDisks_port.t
OP_signal PortAdded : UDisks_port.t

OP_signal ExpanderChanged : UDisks_expander.t
OP_signal ExpanderRemoved : UDisks_expander.t
OP_signal ExpanderAdded : UDisks_expander.t

OP_signal AdapterChanged : UDisks_adapter.t
OP_signal AdapterRemoved : UDisks_adapter.t
OP_signal AdapterAdded : UDisks_adapter.t

OP_signal DeviceChanged : UDisks_device.t
OP_signal DeviceRemoved : UDisks_device.t
OP_signal DeviceAdded : UDisks_device.t

OP_signal DeviceJobChanged : UDisks_device.t * UDisks_device.job

OP_property_r KnownFilesystems : fs structure list
OP_property_r SupportsLuksDevices : bool
OP_property_r DaemonIsInhibited : bool
OP_property_r DaemonVersion : string
