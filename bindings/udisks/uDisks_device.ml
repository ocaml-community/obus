(*
 * uDisks_device.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

include OBus_proxy.Private

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type benchmark_result = {
  bench_read_transfer_rate_results : (int64 * float) list;
  bench_write_transfer_rate_results : (int64 * float) list;
  bench_access_time_results : (int64 * float) list;
}

type spindown_timeout_cookie = string
type inhibit_polling_cookie = string

type process = {
  pr_pid : int;
  pr_uid : int;
  pr_comamnd_line : string;
}

type job = {
  job_in_progress : bool;
  job_id : string;
  job_initiated_by_uid : int;
  job_is_cancellable : bool;
  job_cur_task_percentage : float;
}

(* +-----------------------------------------------------------------+
   | D-Bus members                                                   |
   +-----------------------------------------------------------------+ *)

open UDisks_interfaces.Org_freedesktop_UDisks_Device

let notify_mode = OBus_property.notify_global "Changed"

let job_cancel proxy =
  OBus_method.call m_JobCancel proxy ()

let partition_table_create proxy ~scheme ~options =
  OBus_method.call m_PartitionTableCreate proxy (scheme, options)

let partition_delete proxy ~options =
  OBus_method.call m_PartitionDelete proxy options

let partition_create proxy ~offset ~size ~typ ~label ~flags ~options ~fstype ~fsoptions =
  lwt (context, created_device) = OBus_method.call_with_context m_PartitionCreate proxy (offset, size, typ, label, flags, options, fstype, fsoptions) in
  let created_device = OBus_proxy.make (OBus_context.sender context) created_device in
  return created_device

let partition_modify proxy ~typ ~label ~flags =
  OBus_method.call m_PartitionModify proxy (typ, label, flags)

let filesystem_create proxy ~fstype ~options =
  OBus_method.call m_FilesystemCreate proxy (fstype, options)

let filesystem_set_label proxy ~new_label =
  OBus_method.call m_FilesystemSetLabel proxy new_label

let filesystem_mount proxy ~filesystem_type ~options =
  OBus_method.call m_FilesystemMount proxy (filesystem_type, options)

let filesystem_unmount proxy ~options =
  OBus_method.call m_FilesystemUnmount proxy options

let filesystem_check proxy ~options =
  OBus_method.call m_FilesystemCheck proxy options

let filesystem_list_open_files proxy =
  lwt processes = OBus_method.call m_FilesystemListOpenFiles proxy () in
  return
    (List.map
       (fun (x1, x2, x3) -> {
          pr_pid = Int32.to_int x1;
          pr_uid = Int32.to_int x2;
          pr_comamnd_line = x3;
        })
       processes)

let luks_unlock proxy ~passphrase ~options =
  lwt (context, cleartext_device) = OBus_method.call_with_context m_LuksUnlock proxy (passphrase, options) in
  let cleartext_device = OBus_proxy.make (OBus_context.sender context) cleartext_device in
  return cleartext_device

let luks_lock proxy ~options =
  OBus_method.call m_LuksLock proxy options

let luks_change_passphrase proxy ~current_passphrase ~new_passphrase =
  OBus_method.call m_LuksChangePassphrase proxy (current_passphrase, new_passphrase)

let linux_md_add_spare proxy ~component ~options =
  let component = OBus_proxy.path component in
  OBus_method.call m_LinuxMdAddSpare proxy (component, options)

let linux_md_expand proxy ~components ~options =
  let components = List.map OBus_proxy.path components in
  OBus_method.call m_LinuxMdExpand proxy (components, options)

let linux_md_remove_component proxy ~component ~options =
  let component = OBus_proxy.path component in
  OBus_method.call m_LinuxMdRemoveComponent proxy (component, options)

let linux_md_stop proxy ~options =
  OBus_method.call m_LinuxMdStop proxy options

let linux_lvm2_lvstop proxy ~options =
  OBus_method.call m_LinuxLvm2LVStop proxy options

let linux_md_check proxy ~options =
  OBus_method.call m_LinuxMdCheck proxy options

let drive_inhibit_polling proxy ~options =
  OBus_method.call m_DriveInhibitPolling proxy options

let drive_uninhibit_polling proxy ~cookie =
  OBus_method.call m_DriveUninhibitPolling proxy cookie

let drive_poll_media proxy =
  OBus_method.call m_DrivePollMedia proxy ()

let drive_eject proxy ~options =
  OBus_method.call m_DriveEject proxy options

let drive_detach proxy ~options =
  OBus_method.call m_DriveDetach proxy options

let drive_set_spindown_timeout proxy ~timeout_seconds ~options =
  let timeout_seconds = Int32.of_int timeout_seconds in
  OBus_method.call m_DriveSetSpindownTimeout proxy (timeout_seconds, options)

let drive_unset_spindown_timeout proxy ~cookie =
  OBus_method.call m_DriveUnsetSpindownTimeout proxy cookie

let drive_ata_smart_refresh_data proxy ~options =
  OBus_method.call m_DriveAtaSmartRefreshData proxy options

let drive_ata_smart_initiate_selftest proxy ~test ~options =
  OBus_method.call m_DriveAtaSmartInitiateSelftest proxy (test, options)

let drive_benchmark proxy ~do_write_benchmark ~options =
  lwt (x1, x2, x3) = OBus_method.call m_DriveBenchmark proxy (do_write_benchmark, options) in
  return {
    bench_read_transfer_rate_results = x1;
    bench_write_transfer_rate_results = x2;
    bench_access_time_results = x3;
  }

let changed proxy =
  OBus_signal.connect s_Changed proxy

let job_changed proxy =
  OBus_signal.map
    (fun (job_in_progress, job_is_cancellable, job_id, job_initiated_by_uid, job_percentage) -> {
       job_in_progress = job_in_progress;
       job_id = job_id;
       job_initiated_by_uid = Int32.to_int job_initiated_by_uid;
       job_is_cancellable = job_is_cancellable;
       job_cur_task_percentage = job_percentage;
     })
    (OBus_signal.connect s_JobChanged proxy)

let native_path proxy =
  OBus_property.make p_NativePath ~notify_mode proxy

let device_detection_time proxy =
  OBus_property.make p_DeviceDetectionTime ~notify_mode proxy

let device_media_detection_time proxy =
  OBus_property.make p_DeviceMediaDetectionTime ~notify_mode proxy

let device_major proxy =
  OBus_property.make p_DeviceMajor ~notify_mode proxy

let device_minor proxy =
  OBus_property.make p_DeviceMinor ~notify_mode proxy

let device_file proxy =
  OBus_property.make p_DeviceFile ~notify_mode proxy

let device_file_presentation proxy =
  OBus_property.make p_DeviceFilePresentation ~notify_mode proxy

let device_file_by_id proxy =
  OBus_property.make p_DeviceFileById ~notify_mode proxy

let device_file_by_path proxy =
  OBus_property.make p_DeviceFileByPath ~notify_mode proxy

let device_is_system_internal proxy =
  OBus_property.make p_DeviceIsSystemInternal ~notify_mode proxy

let device_is_partition proxy =
  OBus_property.make p_DeviceIsPartition ~notify_mode proxy

let device_is_partition_table proxy =
  OBus_property.make p_DeviceIsPartitionTable ~notify_mode proxy

let device_is_removable proxy =
  OBus_property.make p_DeviceIsRemovable ~notify_mode proxy

let device_is_media_available proxy =
  OBus_property.make p_DeviceIsMediaAvailable ~notify_mode proxy

let device_is_media_change_detected proxy =
  OBus_property.make p_DeviceIsMediaChangeDetected ~notify_mode proxy

let device_is_media_change_detection_polling proxy =
  OBus_property.make p_DeviceIsMediaChangeDetectionPolling ~notify_mode proxy

let device_is_media_change_detection_inhibitable proxy =
  OBus_property.make p_DeviceIsMediaChangeDetectionInhibitable ~notify_mode proxy

let device_is_media_change_detection_inhibited proxy =
  OBus_property.make p_DeviceIsMediaChangeDetectionInhibited ~notify_mode proxy

let device_is_read_only proxy =
  OBus_property.make p_DeviceIsReadOnly ~notify_mode proxy

let device_is_drive proxy =
  OBus_property.make p_DeviceIsDrive ~notify_mode proxy

let device_is_optical_disc proxy =
  OBus_property.make p_DeviceIsOpticalDisc ~notify_mode proxy

let device_is_mounted proxy =
  OBus_property.make p_DeviceIsMounted ~notify_mode proxy

let device_mount_paths proxy =
  OBus_property.make p_DeviceMountPaths ~notify_mode proxy

let device_mounted_by_uid proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_DeviceMountedByUid ~notify_mode proxy)

let device_is_luks proxy =
  OBus_property.make p_DeviceIsLuks ~notify_mode proxy

let device_is_luks_cleartext proxy =
  OBus_property.make p_DeviceIsLuksCleartext ~notify_mode proxy

let device_is_linux_md_component proxy =
  OBus_property.make p_DeviceIsLinuxMdComponent ~notify_mode proxy

let device_is_linux_md proxy =
  OBus_property.make p_DeviceIsLinuxMd ~notify_mode proxy

let device_is_linux_lvm2_lv proxy =
  OBus_property.make p_DeviceIsLinuxLvm2LV ~notify_mode proxy

let device_is_linux_lvm2_pv proxy =
  OBus_property.make p_DeviceIsLinuxLvm2PV ~notify_mode proxy

let device_is_linux_dmmp_component proxy =
  OBus_property.make p_DeviceIsLinuxDmmpComponent ~notify_mode proxy

let device_is_linux_dmmp proxy =
  OBus_property.make p_DeviceIsLinuxDmmp ~notify_mode proxy

let device_is_linux_loop proxy =
  OBus_property.make p_DeviceIsLinuxLoop ~notify_mode proxy

let device_size proxy =
  OBus_property.make p_DeviceSize ~notify_mode proxy

let device_block_size proxy =
  OBus_property.make p_DeviceBlockSize ~notify_mode proxy

let device_presentation_hide proxy =
  OBus_property.make p_DevicePresentationHide ~notify_mode proxy

let device_presentation_nopolicy proxy =
  OBus_property.make p_DevicePresentationNopolicy ~notify_mode proxy

let device_presentation_name proxy =
  OBus_property.make p_DevicePresentationName ~notify_mode proxy

let device_presentation_icon_name proxy =
  OBus_property.make p_DevicePresentationIconName ~notify_mode proxy

let job_in_progress proxy =
  OBus_property.make p_JobInProgress ~notify_mode proxy

let job_id proxy =
  OBus_property.make p_JobId ~notify_mode proxy

let job_initiated_by_uid proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_JobInitiatedByUid ~notify_mode proxy)

let job_is_cancellable proxy =
  OBus_property.make p_JobIsCancellable ~notify_mode proxy

let job_percentage proxy =
  OBus_property.make p_JobPercentage ~notify_mode proxy

let id_usage proxy =
  OBus_property.make p_IdUsage ~notify_mode proxy

let id_type proxy =
  OBus_property.make p_IdType ~notify_mode proxy

let id_version proxy =
  OBus_property.make p_IdVersion ~notify_mode proxy

let id_uuid proxy =
  OBus_property.make p_IdUuid ~notify_mode proxy

let id_label proxy =
  OBus_property.make p_IdLabel ~notify_mode proxy

let luks_holder proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make p_LuksHolder ~notify_mode proxy)

let luks_cleartext_slave proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make p_LuksCleartextSlave ~notify_mode proxy)

let luks_cleartext_unlocked_by_uid proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_LuksCleartextUnlockedByUid ~notify_mode proxy)

let partition_slave proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make p_PartitionSlave ~notify_mode proxy)

let partition_scheme proxy =
  OBus_property.make p_PartitionScheme ~notify_mode proxy

let partition_type proxy =
  OBus_property.make p_PartitionType ~notify_mode proxy

let partition_label proxy =
  OBus_property.make p_PartitionLabel ~notify_mode proxy

let partition_uuid proxy =
  OBus_property.make p_PartitionUuid ~notify_mode proxy

let partition_flags proxy =
  OBus_property.make p_PartitionFlags ~notify_mode proxy

let partition_number proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_PartitionNumber ~notify_mode proxy)

let partition_offset proxy =
  OBus_property.make p_PartitionOffset ~notify_mode proxy

let partition_size proxy =
  OBus_property.make p_PartitionSize ~notify_mode proxy

let partition_alignment_offset proxy =
  OBus_property.make p_PartitionAlignmentOffset ~notify_mode proxy

let partition_table_scheme proxy =
  OBus_property.make p_PartitionTableScheme ~notify_mode proxy

let partition_table_count proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_PartitionTableCount ~notify_mode proxy)

let drive_vendor proxy =
  OBus_property.make p_DriveVendor ~notify_mode proxy

let drive_model proxy =
  OBus_property.make p_DriveModel ~notify_mode proxy

let drive_revision proxy =
  OBus_property.make p_DriveRevision ~notify_mode proxy

let drive_serial proxy =
  OBus_property.make p_DriveSerial ~notify_mode proxy

let drive_wwn proxy =
  OBus_property.make p_DriveWwn ~notify_mode proxy

let drive_rotation_rate proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_DriveRotationRate ~notify_mode proxy)

let drive_write_cache proxy =
  OBus_property.make p_DriveWriteCache ~notify_mode proxy

let drive_connection_interface proxy =
  OBus_property.make p_DriveConnectionInterface ~notify_mode proxy

let drive_connection_speed proxy =
  OBus_property.make p_DriveConnectionSpeed ~notify_mode proxy

let drive_media_compatibility proxy =
  OBus_property.make p_DriveMediaCompatibility ~notify_mode proxy

let drive_media proxy =
  OBus_property.make p_DriveMedia ~notify_mode proxy

let drive_is_media_ejectable proxy =
  OBus_property.make p_DriveIsMediaEjectable ~notify_mode proxy

let drive_can_detach proxy =
  OBus_property.make p_DriveCanDetach ~notify_mode proxy

let drive_can_spindown proxy =
  OBus_property.make p_DriveCanSpindown ~notify_mode proxy

let drive_is_rotational proxy =
  OBus_property.make p_DriveIsRotational ~notify_mode proxy

let drive_adapter proxy =
  OBus_property.map_r_with_context
    (fun context x -> UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) x))
    (OBus_property.make p_DriveAdapter ~notify_mode proxy)

let drive_ports proxy =
  OBus_property.map_r_with_context
    (fun context x ->
       List.map
         (fun path ->
            UDisks_port.of_proxy (OBus_proxy.make (OBus_context.sender context) path))
         x)
    (OBus_property.make p_DrivePorts ~notify_mode proxy)

let drive_similar_devices proxy =
  OBus_property.map_r_with_context
    (fun context x -> List.map (fun path -> OBus_proxy.make (OBus_context.sender context) path) x)
    (OBus_property.make p_DriveSimilarDevices ~notify_mode proxy)

let optical_disc_is_blank proxy =
  OBus_property.make p_OpticalDiscIsBlank ~notify_mode proxy

let optical_disc_is_appendable proxy =
  OBus_property.make p_OpticalDiscIsAppendable ~notify_mode proxy

let optical_disc_is_closed proxy =
  OBus_property.make p_OpticalDiscIsClosed ~notify_mode proxy

let optical_disc_num_tracks proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_OpticalDiscNumTracks ~notify_mode proxy)

let optical_disc_num_audio_tracks proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_OpticalDiscNumAudioTracks ~notify_mode proxy)

let optical_disc_num_sessions proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_OpticalDiscNumSessions ~notify_mode proxy)

let drive_ata_smart_is_available proxy =
  OBus_property.make p_DriveAtaSmartIsAvailable ~notify_mode proxy

let drive_ata_smart_time_collected proxy =
  OBus_property.make p_DriveAtaSmartTimeCollected ~notify_mode proxy

let drive_ata_smart_status proxy =
  OBus_property.make p_DriveAtaSmartStatus ~notify_mode proxy

let drive_ata_smart_blob proxy =
  OBus_property.make p_DriveAtaSmartBlob ~notify_mode proxy

let linux_md_component_level proxy =
  OBus_property.make p_LinuxMdComponentLevel ~notify_mode proxy

let linux_md_component_position proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_LinuxMdComponentPosition ~notify_mode proxy)

let linux_md_component_num_raid_devices proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_LinuxMdComponentNumRaidDevices ~notify_mode proxy)

let linux_md_component_uuid proxy =
  OBus_property.make p_LinuxMdComponentUuid ~notify_mode proxy

let linux_md_component_name proxy =
  OBus_property.make p_LinuxMdComponentName ~notify_mode proxy

let linux_md_component_home_host proxy =
  OBus_property.make p_LinuxMdComponentHomeHost ~notify_mode proxy

let linux_md_component_version proxy =
  OBus_property.make p_LinuxMdComponentVersion ~notify_mode proxy

let linux_md_component_holder proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make p_LinuxMdComponentHolder ~notify_mode proxy)

let linux_md_component_state proxy =
  OBus_property.make p_LinuxMdComponentState ~notify_mode proxy

let linux_md_state proxy =
  OBus_property.make p_LinuxMdState ~notify_mode proxy

let linux_md_level proxy =
  OBus_property.make p_LinuxMdLevel ~notify_mode proxy

let linux_md_uuid proxy =
  OBus_property.make p_LinuxMdUuid ~notify_mode proxy

let linux_md_home_host proxy =
  OBus_property.make p_LinuxMdHomeHost ~notify_mode proxy

let linux_md_name proxy =
  OBus_property.make p_LinuxMdName ~notify_mode proxy

let linux_md_num_raid_devices proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_LinuxMdNumRaidDevices ~notify_mode proxy)

let linux_md_version proxy =
  OBus_property.make p_LinuxMdVersion ~notify_mode proxy

let linux_md_slaves proxy =
  OBus_property.map_r_with_context
    (fun context x -> List.map (fun path -> OBus_proxy.make (OBus_context.sender context) path) x)
    (OBus_property.make p_LinuxMdSlaves ~notify_mode proxy)

let linux_md_is_degraded proxy =
  OBus_property.make p_LinuxMdIsDegraded ~notify_mode proxy

let linux_md_sync_action proxy =
  OBus_property.make p_LinuxMdSyncAction ~notify_mode proxy

let linux_md_sync_percentage proxy =
  OBus_property.make p_LinuxMdSyncPercentage ~notify_mode proxy

let linux_md_sync_speed proxy =
  OBus_property.make p_LinuxMdSyncSpeed ~notify_mode proxy

let linux_lvm2_pvuuid proxy =
  OBus_property.make p_LinuxLvm2PVUuid ~notify_mode proxy

let linux_lvm2_pvnum_metadata_areas proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make p_LinuxLvm2PVNumMetadataAreas ~notify_mode proxy)

let linux_lvm2_pvgroup_name proxy =
  OBus_property.make p_LinuxLvm2PVGroupName ~notify_mode proxy

let linux_lvm2_pvgroup_uuid proxy =
  OBus_property.make p_LinuxLvm2PVGroupUuid ~notify_mode proxy

let linux_lvm2_pvgroup_size proxy =
  OBus_property.make p_LinuxLvm2PVGroupSize ~notify_mode proxy

let linux_lvm2_pvgroup_unallocated_size proxy =
  OBus_property.make p_LinuxLvm2PVGroupUnallocatedSize ~notify_mode proxy

let linux_lvm2_pvgroup_sequence_number proxy =
  OBus_property.make p_LinuxLvm2PVGroupSequenceNumber ~notify_mode proxy

let linux_lvm2_pvgroup_extent_size proxy =
  OBus_property.make p_LinuxLvm2PVGroupExtentSize ~notify_mode proxy

let linux_lvm2_pvgroup_physical_volumes proxy =
  OBus_property.make p_LinuxLvm2PVGroupPhysicalVolumes ~notify_mode proxy

let linux_lvm2_pvgroup_logical_volumes proxy =
  OBus_property.make p_LinuxLvm2PVGroupLogicalVolumes ~notify_mode proxy

let linux_lvm2_lvname proxy =
  OBus_property.make p_LinuxLvm2LVName ~notify_mode proxy

let linux_lvm2_lvuuid proxy =
  OBus_property.make p_LinuxLvm2LVUuid ~notify_mode proxy

let linux_lvm2_lvgroup_name proxy =
  OBus_property.make p_LinuxLvm2LVGroupName ~notify_mode proxy

let linux_lvm2_lvgroup_uuid proxy =
  OBus_property.make p_LinuxLvm2LVGroupUuid ~notify_mode proxy

let linux_dmmp_component_holder proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make p_LinuxDmmpComponentHolder ~notify_mode proxy)

let linux_dmmp_name proxy =
  OBus_property.make p_LinuxDmmpName ~notify_mode proxy

let linux_dmmp_slaves proxy =
  OBus_property.map_r_with_context
    (fun context x -> List.map (fun path -> OBus_proxy.make (OBus_context.sender context) path) x)
    (OBus_property.make p_LinuxDmmpSlaves ~notify_mode proxy)

let linux_dmmp_parameters proxy =
  OBus_property.make p_LinuxDmmpParameters ~notify_mode proxy

let linux_loop_filename proxy =
  OBus_property.make p_LinuxLoopFilename ~notify_mode proxy

type properties = {
  linux_dmmp_parameters : string;
  linux_dmmp_slaves : t list;
  linux_dmmp_name : string;
  linux_dmmp_component_holder : t;
  linux_lvm2_lvgroup_uuid : string;
  linux_lvm2_lvgroup_name : string;
  linux_lvm2_lvuuid : string;
  linux_lvm2_lvname : string;
  linux_lvm2_pvgroup_logical_volumes : string list;
  linux_lvm2_pvgroup_physical_volumes : string list;
  linux_lvm2_pvgroup_extent_size : int64;
  linux_lvm2_pvgroup_sequence_number : int64;
  linux_lvm2_pvgroup_unallocated_size : int64;
  linux_lvm2_pvgroup_size : int64;
  linux_lvm2_pvgroup_uuid : string;
  linux_lvm2_pvgroup_name : string;
  linux_lvm2_pvnum_metadata_areas : int;
  linux_lvm2_pvuuid : string;
  linux_md_sync_speed : int64;
  linux_md_sync_percentage : float;
  linux_md_sync_action : string;
  linux_md_is_degraded : bool;
  linux_md_slaves : t list;
  linux_md_version : string;
  linux_md_num_raid_devices : int;
  linux_md_name : string;
  linux_md_home_host : string;
  linux_md_uuid : string;
  linux_md_level : string;
  linux_md_state : string;
  linux_md_component_state : string list;
  linux_md_component_holder : t;
  linux_md_component_version : string;
  linux_md_component_home_host : string;
  linux_md_component_name : string;
  linux_md_component_uuid : string;
  linux_md_component_num_raid_devices : int;
  linux_md_component_position : int;
  linux_md_component_level : string;
  drive_ata_smart_blob : string;
  drive_ata_smart_status : string;
  drive_ata_smart_time_collected : int64;
  drive_ata_smart_is_available : bool;
  optical_disc_num_sessions : int;
  optical_disc_num_audio_tracks : int;
  optical_disc_num_tracks : int;
  optical_disc_is_closed : bool;
  optical_disc_is_appendable : bool;
  optical_disc_is_blank : bool;
  drive_similar_devices : t list;
  drive_ports : UDisks_port.t list;
  drive_adapter : UDisks_adapter.t;
  drive_is_rotational : bool;
  drive_can_spindown : bool;
  drive_can_detach : bool;
  drive_is_media_ejectable : bool;
  drive_media : string;
  drive_media_compatibility : string list;
  drive_connection_speed : int64;
  drive_connection_interface : string;
  drive_write_cache : string;
  drive_rotation_rate : int;
  drive_wwn : string;
  drive_serial : string;
  drive_revision : string;
  drive_model : string;
  drive_vendor : string;
  partition_table_count : int;
  partition_table_scheme : string;
  partition_alignment_offset : int64;
  partition_size : int64;
  partition_offset : int64;
  partition_number : int;
  partition_flags : string list;
  partition_uuid : string;
  partition_label : string;
  partition_type : string;
  partition_scheme : string;
  partition_slave : t;
  luks_cleartext_unlocked_by_uid : int;
  luks_cleartext_slave : t;
  luks_holder : t;
  id_label : string;
  id_uuid : string;
  id_version : string;
  id_type : string;
  id_usage : string;
  job_percentage : float;
  job_is_cancellable : bool;
  job_initiated_by_uid : int;
  job_id : string;
  job_in_progress : bool;
  device_presentation_icon_name : string;
  device_presentation_name : string;
  device_presentation_nopolicy : bool;
  device_presentation_hide : bool;
  device_block_size : int64;
  device_size : int64;
  device_is_linux_dmmp : bool;
  device_is_linux_dmmp_component : bool;
  device_is_linux_lvm2_pv : bool;
  device_is_linux_lvm2_lv : bool;
  device_is_linux_md : bool;
  device_is_linux_md_component : bool;
  device_is_luks_cleartext : bool;
  device_is_luks : bool;
  device_mounted_by_uid : int;
  device_mount_paths : string list;
  device_is_mounted : bool;
  device_is_optical_disc : bool;
  device_is_drive : bool;
  device_is_read_only : bool;
  device_is_media_change_detection_inhibited : bool;
  device_is_media_change_detection_inhibitable : bool;
  device_is_media_change_detection_polling : bool;
  device_is_media_change_detected : bool;
  device_is_media_available : bool;
  device_is_removable : bool;
  device_is_partition_table : bool;
  device_is_partition : bool;
  device_is_system_internal : bool;
  device_file_by_path : string list;
  device_file_by_id : string list;
  device_file_presentation : string;
  device_file : string;
  device_minor : int64;
  device_major : int64;
  device_media_detection_time : int64;
  device_detection_time : int64;
  native_path : string;
}

let properties proxy =
  OBus_property.map_r_with_context
    (fun context properties ->
       let find f = OBus_property.find (f proxy) context properties in
       {
         linux_dmmp_parameters = find linux_dmmp_parameters;
         linux_dmmp_slaves = find linux_dmmp_slaves;
         linux_dmmp_name = find linux_dmmp_name;
         linux_dmmp_component_holder = find linux_dmmp_component_holder;
         linux_lvm2_lvgroup_uuid = find linux_lvm2_lvgroup_uuid;
         linux_lvm2_lvgroup_name = find linux_lvm2_lvgroup_name;
         linux_lvm2_lvuuid = find linux_lvm2_lvuuid;
         linux_lvm2_lvname = find linux_lvm2_lvname;
         linux_lvm2_pvgroup_logical_volumes = find linux_lvm2_pvgroup_logical_volumes;
         linux_lvm2_pvgroup_physical_volumes = find linux_lvm2_pvgroup_physical_volumes;
         linux_lvm2_pvgroup_extent_size = find linux_lvm2_pvgroup_extent_size;
         linux_lvm2_pvgroup_sequence_number = find linux_lvm2_pvgroup_sequence_number;
         linux_lvm2_pvgroup_unallocated_size = find linux_lvm2_pvgroup_unallocated_size;
         linux_lvm2_pvgroup_size = find linux_lvm2_pvgroup_size;
         linux_lvm2_pvgroup_uuid = find linux_lvm2_pvgroup_uuid;
         linux_lvm2_pvgroup_name = find linux_lvm2_pvgroup_name;
         linux_lvm2_pvnum_metadata_areas = find linux_lvm2_pvnum_metadata_areas;
         linux_lvm2_pvuuid = find linux_lvm2_pvuuid;
         linux_md_sync_speed = find linux_md_sync_speed;
         linux_md_sync_percentage = find linux_md_sync_percentage;
         linux_md_sync_action = find linux_md_sync_action;
         linux_md_is_degraded = find linux_md_is_degraded;
         linux_md_slaves = find linux_md_slaves;
         linux_md_version = find linux_md_version;
         linux_md_num_raid_devices = find linux_md_num_raid_devices;
         linux_md_name = find linux_md_name;
         linux_md_home_host = find linux_md_home_host;
         linux_md_uuid = find linux_md_uuid;
         linux_md_level = find linux_md_level;
         linux_md_state = find linux_md_state;
         linux_md_component_state = find linux_md_component_state;
         linux_md_component_holder = find linux_md_component_holder;
         linux_md_component_version = find linux_md_component_version;
         linux_md_component_home_host = find linux_md_component_home_host;
         linux_md_component_name = find linux_md_component_name;
         linux_md_component_uuid = find linux_md_component_uuid;
         linux_md_component_num_raid_devices = find linux_md_component_num_raid_devices;
         linux_md_component_position = find linux_md_component_position;
         linux_md_component_level = find linux_md_component_level;
         drive_ata_smart_blob = find drive_ata_smart_blob;
         drive_ata_smart_status = find drive_ata_smart_status;
         drive_ata_smart_time_collected = find drive_ata_smart_time_collected;
         drive_ata_smart_is_available = find drive_ata_smart_is_available;
         optical_disc_num_sessions = find optical_disc_num_sessions;
         optical_disc_num_audio_tracks = find optical_disc_num_audio_tracks;
         optical_disc_num_tracks = find optical_disc_num_tracks;
         optical_disc_is_closed = find optical_disc_is_closed;
         optical_disc_is_appendable = find optical_disc_is_appendable;
         optical_disc_is_blank = find optical_disc_is_blank;
         drive_similar_devices = find drive_similar_devices;
         drive_ports = find drive_ports;
         drive_adapter = find drive_adapter;
         drive_is_rotational = find drive_is_rotational;
         drive_can_spindown = find drive_can_spindown;
         drive_can_detach = find drive_can_detach;
         drive_is_media_ejectable = find drive_is_media_ejectable;
         drive_media = find drive_media;
         drive_media_compatibility = find drive_media_compatibility;
         drive_connection_speed = find drive_connection_speed;
         drive_connection_interface = find drive_connection_interface;
         drive_write_cache = find drive_write_cache;
         drive_rotation_rate = find drive_rotation_rate;
         drive_wwn = find drive_wwn;
         drive_serial = find drive_serial;
         drive_revision = find drive_revision;
         drive_model = find drive_model;
         drive_vendor = find drive_vendor;
         partition_table_count = find partition_table_count;
         partition_table_scheme = find partition_table_scheme;
         partition_alignment_offset = find partition_alignment_offset;
         partition_size = find partition_size;
         partition_offset = find partition_offset;
         partition_number = find partition_number;
         partition_flags = find partition_flags;
         partition_uuid = find partition_uuid;
         partition_label = find partition_label;
         partition_type = find partition_type;
         partition_scheme = find partition_scheme;
         partition_slave = find partition_slave;
         luks_cleartext_unlocked_by_uid = find luks_cleartext_unlocked_by_uid;
         luks_cleartext_slave = find luks_cleartext_slave;
         luks_holder = find luks_holder;
         id_label = find id_label;
         id_uuid = find id_uuid;
         id_version = find id_version;
         id_type = find id_type;
         id_usage = find id_usage;
         job_percentage = find job_percentage;
         job_is_cancellable = find job_is_cancellable;
         job_initiated_by_uid = find job_initiated_by_uid;
         job_id = find job_id;
         job_in_progress = find job_in_progress;
         device_presentation_icon_name = find device_presentation_icon_name;
         device_presentation_name = find device_presentation_name;
         device_presentation_nopolicy = find device_presentation_nopolicy;
         device_presentation_hide = find device_presentation_hide;
         device_block_size = find device_block_size;
         device_size = find device_size;
         device_is_linux_dmmp = find device_is_linux_dmmp;
         device_is_linux_dmmp_component = find device_is_linux_dmmp_component;
         device_is_linux_lvm2_pv = find device_is_linux_lvm2_pv;
         device_is_linux_lvm2_lv = find device_is_linux_lvm2_lv;
         device_is_linux_md = find device_is_linux_md;
         device_is_linux_md_component = find device_is_linux_md_component;
         device_is_luks_cleartext = find device_is_luks_cleartext;
         device_is_luks = find device_is_luks;
         device_mounted_by_uid = find device_mounted_by_uid;
         device_mount_paths = find device_mount_paths;
         device_is_mounted = find device_is_mounted;
         device_is_optical_disc = find device_is_optical_disc;
         device_is_drive = find device_is_drive;
         device_is_read_only = find device_is_read_only;
         device_is_media_change_detection_inhibited = find device_is_media_change_detection_inhibited;
         device_is_media_change_detection_inhibitable = find device_is_media_change_detection_inhibitable;
         device_is_media_change_detection_polling = find device_is_media_change_detection_polling;
         device_is_media_change_detected = find device_is_media_change_detected;
         device_is_media_available = find device_is_media_available;
         device_is_removable = find device_is_removable;
         device_is_partition_table = find device_is_partition_table;
         device_is_partition = find device_is_partition;
         device_is_system_internal = find device_is_system_internal;
         device_file_by_path = find device_file_by_path;
         device_file_by_id = find device_file_by_id;
         device_file_presentation = find device_file_presentation;
         device_file = find device_file;
         device_minor = find device_minor;
         device_major = find device_major;
         device_media_detection_time = find device_media_detection_time;
         device_detection_time = find device_detection_time;
         native_path = find native_path;
       })
    (OBus_property.make_group proxy ~notify_mode interface)

