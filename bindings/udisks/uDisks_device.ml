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

let job_cancel proxy =
  OBus_method.call m_JobCancel proxy ()

let partition_table_create proxy ~scheme ~options =
  OBus_method.call m_PartitionTableCreate proxy (scheme, options)

let partition_delete proxy ~options =
  OBus_method.call m_PartitionDelete proxy options

let partition_create proxy ~offset ~size ~typ ~label ~flags ~options ~fstype ~fsoptions =
  let%lwt (context, created_device) = OBus_method.call_with_context m_PartitionCreate proxy (offset, size, typ, label, flags, options, fstype, fsoptions) in
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
  let%lwt processes = OBus_method.call m_FilesystemListOpenFiles proxy () in
  return
    (List.map
       (fun (x1, x2, x3) -> {
          pr_pid = Int32.to_int x1;
          pr_uid = Int32.to_int x2;
          pr_comamnd_line = x3;
        })
       processes)

let luks_unlock proxy ~passphrase ~options =
  let%lwt (context, cleartext_device) = OBus_method.call_with_context m_LuksUnlock proxy (passphrase, options) in
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
  let%lwt (x1, x2, x3) = OBus_method.call m_DriveBenchmark proxy (do_write_benchmark, options) in
  return {
    bench_read_transfer_rate_results = x1;
    bench_write_transfer_rate_results = x2;
    bench_access_time_results = x3;
  }

let changed proxy =
  OBus_signal.make s_Changed proxy

let job_changed proxy =
  OBus_signal.map
    (fun (job_in_progress, job_is_cancellable, job_id, job_initiated_by_uid, job_percentage) -> {
       job_in_progress = job_in_progress;
       job_id = job_id;
       job_initiated_by_uid = Int32.to_int job_initiated_by_uid;
       job_is_cancellable = job_is_cancellable;
       job_cur_task_percentage = job_percentage;
     })
    (OBus_signal.make s_JobChanged proxy)

let native_path proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_NativePath proxy

let device_detection_time proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceDetectionTime proxy

let device_media_detection_time proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceMediaDetectionTime proxy

let device_major proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceMajor proxy

let device_minor proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceMinor proxy

let device_file proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceFile proxy

let device_file_presentation proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceFilePresentation proxy

let device_file_by_id proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceFileById proxy

let device_file_by_path proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceFileByPath proxy

let device_is_system_internal proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsSystemInternal proxy

let device_is_partition proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsPartition proxy

let device_is_partition_table proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsPartitionTable proxy

let device_is_removable proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsRemovable proxy

let device_is_media_available proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsMediaAvailable proxy

let device_is_media_change_detected proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsMediaChangeDetected proxy

let device_is_media_change_detection_polling proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsMediaChangeDetectionPolling proxy

let device_is_media_change_detection_inhibitable proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsMediaChangeDetectionInhibitable proxy

let device_is_media_change_detection_inhibited proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsMediaChangeDetectionInhibited proxy

let device_is_read_only proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsReadOnly proxy

let device_is_drive proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsDrive proxy

let device_is_optical_disc proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsOpticalDisc proxy

let device_is_mounted proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsMounted proxy

let device_mount_paths proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceMountPaths proxy

let device_mounted_by_uid proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceMountedByUid proxy)

let device_is_luks proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLuks proxy

let device_is_luks_cleartext proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLuksCleartext proxy

let device_is_linux_md_component proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLinuxMdComponent proxy

let device_is_linux_md proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLinuxMd proxy

let device_is_linux_lvm2_lv proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLinuxLvm2LV proxy

let device_is_linux_lvm2_pv proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLinuxLvm2PV proxy

let device_is_linux_dmmp_component proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLinuxDmmpComponent proxy

let device_is_linux_dmmp proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLinuxDmmp proxy

let device_is_linux_loop proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceIsLinuxLoop proxy

let device_size proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceSize proxy

let device_block_size proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DeviceBlockSize proxy

let device_presentation_hide proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DevicePresentationHide proxy

let device_presentation_nopolicy proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DevicePresentationNopolicy proxy

let device_presentation_name proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DevicePresentationName proxy

let device_presentation_icon_name proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DevicePresentationIconName proxy

let job_in_progress proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_JobInProgress proxy

let job_id proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_JobId proxy

let job_initiated_by_uid proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_JobInitiatedByUid proxy)

let job_is_cancellable proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_JobIsCancellable proxy

let job_percentage proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_JobPercentage proxy

let id_usage proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_IdUsage proxy

let id_type proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_IdType proxy

let id_version proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_IdVersion proxy

let id_uuid proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_IdUuid proxy

let id_label proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_IdLabel proxy

let luks_holder proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LuksHolder proxy)

let luks_cleartext_slave proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LuksCleartextSlave proxy)

let luks_cleartext_unlocked_by_uid proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LuksCleartextUnlockedByUid proxy)

let partition_slave proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionSlave proxy)

let partition_scheme proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionScheme proxy

let partition_type proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionType proxy

let partition_label proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionLabel proxy

let partition_uuid proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionUuid proxy

let partition_flags proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionFlags proxy

let partition_number proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionNumber proxy)

let partition_offset proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionOffset proxy

let partition_size proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionSize proxy

let partition_alignment_offset proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionAlignmentOffset proxy

let partition_table_scheme proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionTableScheme proxy

let partition_table_count proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_PartitionTableCount proxy)

let drive_vendor proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveVendor proxy

let drive_model proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveModel proxy

let drive_revision proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveRevision proxy

let drive_serial proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveSerial proxy

let drive_wwn proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveWwn proxy

let drive_rotation_rate proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveRotationRate proxy)

let drive_write_cache proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveWriteCache proxy

let drive_connection_interface proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveConnectionInterface proxy

let drive_connection_speed proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveConnectionSpeed proxy

let drive_media_compatibility proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveMediaCompatibility proxy

let drive_media proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveMedia proxy

let drive_is_media_ejectable proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveIsMediaEjectable proxy

let drive_can_detach proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveCanDetach proxy

let drive_can_spindown proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveCanSpindown proxy

let drive_is_rotational proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveIsRotational proxy

let drive_adapter proxy =
  OBus_property.map_r_with_context
    (fun context x -> UDisks_adapter.of_proxy (OBus_proxy.make (OBus_context.sender context) x))
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveAdapter proxy)

let drive_ports proxy =
  OBus_property.map_r_with_context
    (fun context x ->
       List.map
         (fun path ->
            UDisks_port.of_proxy (OBus_proxy.make (OBus_context.sender context) path))
         x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_DrivePorts proxy)

let drive_similar_devices proxy =
  OBus_property.map_r_with_context
    (fun context x -> List.map (fun path -> OBus_proxy.make (OBus_context.sender context) path) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveSimilarDevices proxy)

let optical_disc_is_blank proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_OpticalDiscIsBlank proxy

let optical_disc_is_appendable proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_OpticalDiscIsAppendable proxy

let optical_disc_is_closed proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_OpticalDiscIsClosed proxy

let optical_disc_num_tracks proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_OpticalDiscNumTracks proxy)

let optical_disc_num_audio_tracks proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_OpticalDiscNumAudioTracks proxy)

let optical_disc_num_sessions proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_OpticalDiscNumSessions proxy)

let drive_ata_smart_is_available proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveAtaSmartIsAvailable proxy

let drive_ata_smart_time_collected proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveAtaSmartTimeCollected proxy

let drive_ata_smart_status proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveAtaSmartStatus proxy

let drive_ata_smart_blob proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_DriveAtaSmartBlob proxy

let linux_md_component_level proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentLevel proxy

let linux_md_component_position proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentPosition proxy)

let linux_md_component_num_raid_devices proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentNumRaidDevices proxy)

let linux_md_component_uuid proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentUuid proxy

let linux_md_component_name proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentName proxy

let linux_md_component_home_host proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentHomeHost proxy

let linux_md_component_version proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentVersion proxy

let linux_md_component_holder proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentHolder proxy)

let linux_md_component_state proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdComponentState proxy

let linux_md_state proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdState proxy

let linux_md_level proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdLevel proxy

let linux_md_uuid proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdUuid proxy

let linux_md_home_host proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdHomeHost proxy

let linux_md_name proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdName proxy

let linux_md_num_raid_devices proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdNumRaidDevices proxy)

let linux_md_version proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdVersion proxy

let linux_md_slaves proxy =
  OBus_property.map_r_with_context
    (fun context x -> List.map (fun path -> OBus_proxy.make (OBus_context.sender context) path) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdSlaves proxy)

let linux_md_is_degraded proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdIsDegraded proxy

let linux_md_sync_action proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdSyncAction proxy

let linux_md_sync_percentage proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdSyncPercentage proxy

let linux_md_sync_speed proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxMdSyncSpeed proxy

let linux_lvm2_pvuuid proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVUuid proxy

let linux_lvm2_pvnum_metadata_areas proxy =
  OBus_property.map_r
    (fun x -> Int32.to_int x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVNumMetadataAreas proxy)

let linux_lvm2_pvgroup_name proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVGroupName proxy

let linux_lvm2_pvgroup_uuid proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVGroupUuid proxy

let linux_lvm2_pvgroup_size proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVGroupSize proxy

let linux_lvm2_pvgroup_unallocated_size proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVGroupUnallocatedSize proxy

let linux_lvm2_pvgroup_sequence_number proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVGroupSequenceNumber proxy

let linux_lvm2_pvgroup_extent_size proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVGroupExtentSize proxy

let linux_lvm2_pvgroup_physical_volumes proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVGroupPhysicalVolumes proxy

let linux_lvm2_pvgroup_logical_volumes proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2PVGroupLogicalVolumes proxy

let linux_lvm2_lvname proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2LVName proxy

let linux_lvm2_lvuuid proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2LVUuid proxy

let linux_lvm2_lvgroup_name proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2LVGroupName proxy

let linux_lvm2_lvgroup_uuid proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLvm2LVGroupUuid proxy

let linux_dmmp_component_holder proxy =
  OBus_property.map_r_with_context
    (fun context x -> OBus_proxy.make (OBus_context.sender context) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxDmmpComponentHolder proxy)

let linux_dmmp_name proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxDmmpName proxy

let linux_dmmp_slaves proxy =
  OBus_property.map_r_with_context
    (fun context x -> List.map (fun path -> OBus_proxy.make (OBus_context.sender context) path) x)
    (OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxDmmpSlaves proxy)

let linux_dmmp_parameters proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxDmmpParameters proxy

let linux_loop_filename proxy =
  OBus_property.make ~monitor:UDisks_monitor.monitor p_LinuxLoopFilename proxy

let properties proxy =
  OBus_property.group ~monitor:UDisks_monitor.monitor proxy interface

