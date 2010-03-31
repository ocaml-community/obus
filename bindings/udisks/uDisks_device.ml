(*
 * uDisks_device.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_pervasives

include OBus_proxy.Private

let op_interface = OBus_proxy.make_interface ~changed:"Changed" "org.freedesktop.UDisks.Device"

type benchmark_result = {
  bench_read_transfer_rate_results : (uint64 * float) structure list;
  bench_write_transfer_rate_results : (uint64 * float) structure list;
  bench_access_time_results : (uint64 * float) structure list;
} with obus

OP_method DriveBenchmark : do_write_benchmark : bool -> options : string list -> benchmark_result
OP_method DriveAtaSmartInitiateSelftest : test : string -> options : string list -> unit
OP_method DriveAtaSmartRefreshData : options : string list -> unit

type spindown_timeout_cookie = string with obus

OP_method DriveUnsetSpindownTimeout : cookie : spindown_timeout_cookie -> unit
OP_method DriveSetSpindownTimeout : timeout_seconds : int -> options : string list -> spindown_timeout_cookie

OP_method DriveDetach : options : string list -> unit
OP_method DriveEject : options : string list -> unit
OP_method DrivePollMedia : unit

type inhibit_polling_cookie = string with obus

OP_method DriveInhibitPolling : options : string list -> inhibit_polling_cookie
OP_method DriveUninhibitPolling : cookie : inhibit_polling_cookie -> unit

OP_method LinuxMdCheck : options : string list -> uint64
OP_method LinuxLvm2LVStop : options : string list -> unit
OP_method LinuxMdStop : options : string list -> unit
OP_method LinuxMdRemoveComponent : component : OBus_proxy.t -> options : string list -> unit
OP_method LinuxMdExpand : components : OBus_proxy.t list -> options : string list -> unit
OP_method LinuxMdAddSpare : component : OBus_proxy.t -> options : string list -> unit
OP_method LuksChangePassphrase : current_passphrase : string -> new_passphrase : string -> unit
OP_method LuksLock : options : string list -> unit
OP_method LuksUnlock : passphrase : string -> options : string list -> OBus_proxy.t

type process = {
  pr_pid : uint;
  pr_uid : uint;
  pr_comamnd_line : string;
} with obus

OP_method FilesystemListOpenFiles : process structure list
OP_method FilesystemCheck : options : string list -> bool
OP_method FilesystemUnmount : options : string list -> unit
OP_method FilesystemMount : filesystem_type : string -> options : string list -> string
OP_method FilesystemSetLabel : new_label : string -> unit
OP_method FilesystemCreate : fstype : string -> options : string list -> unit

OP_method PartitionModify : typ : string -> label : string -> flags : string list -> unit
OP_method PartitionCreate : offset : uint64 -> size : uint64 -> typ : string -> label : string -> flags : string list -> options : string list -> fstype : string -> fsoptions : string list -> OBus_proxy.t
OP_method PartitionDelete : options : string list -> unit
OP_method PartitionTableCreate : scheme : string -> options : string list -> unit

OP_method JobCancel : unit

type job = {
  job_in_progress : bool;
  job_id : string;
  job_initiated_by_uid : uint;
  job_is_cancellable : bool;
  job_cur_task_percentage : float;
} with obus

OP_signal JobChanged : job
OP_signal Changed : unit

OP_property_r LinuxDmmpParameters : string
OP_property_r LinuxDmmpSlaves : t list
OP_property_r LinuxDmmpName : string
OP_property_r LinuxDmmpComponentHolder : t
OP_property_r LinuxLvm2LVGroupUuid : string
OP_property_r LinuxLvm2LVGroupName : string
OP_property_r LinuxLvm2LVUuid : string
OP_property_r LinuxLvm2LVName : string
OP_property_r LinuxLvm2PVGroupLogicalVolumes : string list
OP_property_r LinuxLvm2PVGroupPhysicalVolumes : string list
OP_property_r LinuxLvm2PVGroupExtentSize : uint64
OP_property_r LinuxLvm2PVGroupSequenceNumber : uint64
OP_property_r LinuxLvm2PVGroupUnallocatedSize : uint64
OP_property_r LinuxLvm2PVGroupSize : uint64
OP_property_r LinuxLvm2PVGroupUuid : string
OP_property_r LinuxLvm2PVGroupName : string
OP_property_r LinuxLvm2PVNumMetadataAreas : uint
OP_property_r LinuxLvm2PVUuid : string
OP_property_r LinuxMdSyncSpeed : uint64
OP_property_r LinuxMdSyncPercentage : float
OP_property_r LinuxMdSyncAction : string
OP_property_r LinuxMdIsDegraded : bool
OP_property_r LinuxMdSlaves : t list
OP_property_r LinuxMdVersion : string
OP_property_r LinuxMdNumRaidDevices : int
OP_property_r LinuxMdName : string
OP_property_r LinuxMdHomeHost : string
OP_property_r LinuxMdUuid : string
OP_property_r LinuxMdLevel : string
OP_property_r LinuxMdState : string
OP_property_r LinuxMdComponentState : string list
OP_property_r LinuxMdComponentHolder : t
OP_property_r LinuxMdComponentVersion : string
OP_property_r LinuxMdComponentHomeHost : string
OP_property_r LinuxMdComponentName : string
OP_property_r LinuxMdComponentUuid : string
OP_property_r LinuxMdComponentNumRaidDevices : int
OP_property_r LinuxMdComponentPosition : int
OP_property_r LinuxMdComponentLevel : string
OP_property_r DriveAtaSmartBlob : char list
OP_property_r DriveAtaSmartStatus : string
OP_property_r DriveAtaSmartTimeCollected : uint64
OP_property_r DriveAtaSmartIsAvailable : bool
OP_property_r OpticalDiscNumSessions : uint
OP_property_r OpticalDiscNumAudioTracks : uint
OP_property_r OpticalDiscNumTracks : uint
OP_property_r OpticalDiscIsClosed : bool
OP_property_r OpticalDiscIsAppendable : bool
OP_property_r OpticalDiscIsBlank : bool
OP_property_r DriveSimilarDevices : t list
OP_property_r DrivePorts : UDisks_port.t list
OP_property_r DriveAdapter : UDisks_adapter.t
OP_property_r DriveIsRotational : bool
OP_property_r DriveCanSpindown : bool
OP_property_r DriveCanDetach : bool
OP_property_r DriveIsMediaEjectable : bool
OP_property_r DriveMedia : string
OP_property_r DriveMediaCompatibility : string list
OP_property_r DriveConnectionSpeed : uint64
OP_property_r DriveConnectionInterface : string
OP_property_r DriveWriteCache : string
OP_property_r DriveRotationRate : uint
OP_property_r DriveWwn : string
OP_property_r DriveSerial : string
OP_property_r DriveRevision : string
OP_property_r DriveModel : string
OP_property_r DriveVendor : string
OP_property_r PartitionTableCount : int
OP_property_r PartitionTableScheme : string
OP_property_r PartitionAlignmentOffset : uint64
OP_property_r PartitionSize : uint64
OP_property_r PartitionOffset : uint64
OP_property_r PartitionNumber : int
OP_property_r PartitionFlags : string list
OP_property_r PartitionUuid : string
OP_property_r PartitionLabel : string
OP_property_r PartitionType : string
OP_property_r PartitionScheme : string
OP_property_r PartitionSlave : t
OP_property_r LuksCleartextUnlockedByUid : uint
OP_property_r LuksCleartextSlave : t
OP_property_r LuksHolder : t
OP_property_r IdLabel : string
OP_property_r IdUuid : string
OP_property_r IdVersion : string
OP_property_r IdType : string
OP_property_r IdUsage : string
OP_property_r JobPercentage : float
OP_property_r JobIsCancellable : bool
OP_property_r JobInitiatedByUid : uint
OP_property_r JobId : string
OP_property_r JobInProgress : bool
OP_property_r DevicePresentationIconName : string
OP_property_r DevicePresentationName : string
OP_property_r DevicePresentationNopolicy : bool
OP_property_r DevicePresentationHide : bool
OP_property_r DeviceBlockSize : uint64
OP_property_r DeviceSize : uint64
OP_property_r DeviceIsLinuxDmmp : bool
OP_property_r DeviceIsLinuxDmmpComponent : bool
OP_property_r DeviceIsLinuxLvm2PV : bool
OP_property_r DeviceIsLinuxLvm2LV : bool
OP_property_r DeviceIsLinuxMd : bool
OP_property_r DeviceIsLinuxMdComponent : bool
OP_property_r DeviceIsLuksCleartext : bool
OP_property_r DeviceIsLuks : bool
OP_property_r DeviceMountedByUid : uint
OP_property_r DeviceMountPaths : string list
OP_property_r DeviceIsMounted : bool
OP_property_r DeviceIsOpticalDisc : bool
OP_property_r DeviceIsDrive : bool
OP_property_r DeviceIsReadOnly : bool
OP_property_r DeviceIsMediaChangeDetectionInhibited : bool
OP_property_r DeviceIsMediaChangeDetectionInhibitable : bool
OP_property_r DeviceIsMediaChangeDetectionPolling : bool
OP_property_r DeviceIsMediaChangeDetected : bool
OP_property_r DeviceIsMediaAvailable : bool
OP_property_r DeviceIsRemovable : bool
OP_property_r DeviceIsPartitionTable : bool
OP_property_r DeviceIsPartition : bool
OP_property_r DeviceIsSystemInternal : bool
OP_property_r DeviceFileByPath : string list
OP_property_r DeviceFileById : string list
OP_property_r DeviceFilePresentation : string
OP_property_r DeviceFile : string
OP_property_r DeviceMinor : int64
OP_property_r DeviceMajor : int64
OP_property_r DeviceMediaDetectionTime : uint64
OP_property_r DeviceDetectionTime : uint64
OP_property_r NativePath : string
