(*
 * uDisks_device.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** UDisks device interface *)

include OBus_proxy.Private

(** {6 Methods} *)

type benchmark_result = {
  bench_read_transfer_rate_results : (int64 * float) list;
  (** An array of pairs where the first element is the offset and the
      second element is the measured read transfer rate (in bytes/sec)
      at the given offset. *)
  bench_write_transfer_rate_results : (int64 * float) list;
  (** An array of pairs where the first element is the offset and the
      second element is the measured read transfer rate (in bytes/sec)
      at the given offset. This is an empty array unless write
      benchmarking has been requested. *)
  bench_access_time_results : (int64 * float) list;
  (** An array of pairs where the first element is the offset and the
      second element the amount of time (in seconds) it took to seek
      to the position. *)
}

val drive_benchmark : t -> do_write_benchmark : bool -> options : string list -> benchmark_result Lwt.t

val drive_ata_smart_initiate_selftest : t -> test : string -> options : string list -> unit Lwt.t
val drive_ata_smart_refresh_data : t -> options : string list -> unit Lwt.t

type spindown_timeout_cookie

val drive_set_spindown_timeout : t -> timeout_seconds : int -> options : string list -> spindown_timeout_cookie Lwt.t
val drive_unset_spindown_timeout : t -> cookie : spindown_timeout_cookie -> unit Lwt.t

val drive_detach : t -> options : string list -> unit Lwt.t
val drive_eject : t -> options : string list -> unit Lwt.t
val drive_poll_media : t -> unit Lwt.t

type inhibit_polling_cookie

val drive_inhibit_polling : t -> options : string list -> inhibit_polling_cookie Lwt.t
val drive_uninhibit_polling : t -> cookie : inhibit_polling_cookie -> unit Lwt.t

val linux_md_check : t -> options : string list -> int64 Lwt.t
val linux_lvm2_lvstop : t -> options : string list -> unit Lwt.t

val linux_md_stop : t -> options : string list -> unit Lwt.t
val linux_md_remove_component : t -> component : t -> options : string list -> unit Lwt.t
val linux_md_expand : t -> components : t list -> options : string list -> unit Lwt.t
val linux_md_add_spare : t -> component : t -> options : string list -> unit Lwt.t

val luks_change_passphrase : t -> current_passphrase : string -> new_passphrase : string -> unit Lwt.t
val luks_lock : t -> options : string list -> unit Lwt.t
val luks_unlock : t -> passphrase : string -> options : string list -> t Lwt.t

type process = {
  pr_pid : int;
  pr_uid : int;
  pr_comamnd_line : string;
}

val filesystem_list_open_files : t -> process list Lwt.t
val filesystem_check : t -> options : string list -> bool Lwt.t
val filesystem_unmount : t -> options : string list -> unit Lwt.t
val filesystem_mount : t -> filesystem_type : string -> options : string list -> string Lwt.t
val filesystem_set_label : t -> new_label : string -> unit Lwt.t
val filesystem_create : t -> fstype : string -> options : string list -> unit Lwt.t

val partition_modify : t -> typ : string -> label : string -> flags : string list -> unit Lwt.t
val partition_create : t -> offset : int64 -> size : int64 -> typ : string -> label : string -> flags : string list -> options : string list -> fstype : string -> fsoptions : string list -> t Lwt.t
val partition_delete : t -> options : string list -> unit Lwt.t
val partition_table_create : t -> scheme : string -> options : string list -> unit Lwt.t

val job_cancel : t -> unit Lwt.t

(** {6 Signals} *)

(** A job description *)
type job = {
  job_in_progress : bool;
  (** Whether a job is currently in progress *)

  job_id : string;
  (** The identifier of the job *)

  job_initiated_by_uid : int;
  (** he UNIX user id of the user who initiated the job *)

  job_is_cancellable : bool;
  (** Whether the job is cancellable *)

  job_cur_task_percentage : float;
  (** Percentage completed of current task (between 0 and 100,
      negative if unknown) *)
}

val job_changed : t -> job OBus_signal.t
val changed : t -> unit OBus_signal.t

(** {6 Properties} *)

val linux_dmmp_parameters : t -> string OBus_property.r
val linux_dmmp_slaves : t -> t list OBus_property.r
val linux_dmmp_name : t -> string OBus_property.r
val linux_dmmp_component_holder : t -> t OBus_property.r
val linux_lvm2_lvgroup_uuid : t -> string OBus_property.r
val linux_lvm2_lvgroup_name : t -> string OBus_property.r
val linux_lvm2_lvuuid : t -> string OBus_property.r
val linux_lvm2_lvname : t -> string OBus_property.r
val linux_lvm2_pvgroup_logical_volumes : t -> string list OBus_property.r
val linux_lvm2_pvgroup_physical_volumes : t -> string list OBus_property.r
val linux_lvm2_pvgroup_extent_size : t -> int64 OBus_property.r
val linux_lvm2_pvgroup_sequence_number : t -> int64 OBus_property.r
val linux_lvm2_pvgroup_unallocated_size : t -> int64 OBus_property.r
val linux_lvm2_pvgroup_size : t -> int64 OBus_property.r
val linux_lvm2_pvgroup_uuid : t -> string OBus_property.r
val linux_lvm2_pvgroup_name : t -> string OBus_property.r
val linux_lvm2_pvnum_metadata_areas : t -> int OBus_property.r
val linux_lvm2_pvuuid : t -> string OBus_property.r
val linux_md_sync_speed : t -> int64 OBus_property.r
val linux_md_sync_percentage : t -> float OBus_property.r
val linux_md_sync_action : t -> string OBus_property.r
val linux_md_is_degraded : t -> bool OBus_property.r
val linux_md_slaves : t -> t list OBus_property.r
val linux_md_version : t -> string OBus_property.r
val linux_md_num_raid_devices : t -> int OBus_property.r
val linux_md_name : t -> string OBus_property.r
val linux_md_home_host : t -> string OBus_property.r
val linux_md_uuid : t -> string OBus_property.r
val linux_md_level : t -> string OBus_property.r
val linux_md_state : t -> string OBus_property.r
val linux_md_component_state : t -> string list OBus_property.r
val linux_md_component_holder : t -> t OBus_property.r
val linux_md_component_version : t -> string OBus_property.r
val linux_md_component_home_host : t -> string OBus_property.r
val linux_md_component_name : t -> string OBus_property.r
val linux_md_component_uuid : t -> string OBus_property.r
val linux_md_component_num_raid_devices : t -> int OBus_property.r
val linux_md_component_position : t -> int OBus_property.r
val linux_md_component_level : t -> string OBus_property.r
val drive_ata_smart_blob : t -> string OBus_property.r
val drive_ata_smart_status : t -> string OBus_property.r
val drive_ata_smart_time_collected : t -> int64 OBus_property.r
val drive_ata_smart_is_available : t -> bool OBus_property.r
val optical_disc_num_sessions : t -> int OBus_property.r
val optical_disc_num_audio_tracks : t -> int OBus_property.r
val optical_disc_num_tracks : t -> int OBus_property.r
val optical_disc_is_closed : t -> bool OBus_property.r
val optical_disc_is_appendable : t -> bool OBus_property.r
val optical_disc_is_blank : t -> bool OBus_property.r
val drive_similar_devices : t -> t list OBus_property.r
val drive_ports : t -> UDisks_port.t list OBus_property.r
val drive_adapter : t -> UDisks_adapter.t OBus_property.r
val drive_is_rotational : t -> bool OBus_property.r
val drive_can_spindown : t -> bool OBus_property.r
val drive_can_detach : t -> bool OBus_property.r
val drive_is_media_ejectable : t -> bool OBus_property.r
val drive_media : t -> string OBus_property.r
val drive_media_compatibility : t -> string list OBus_property.r
val drive_connection_speed : t -> int64 OBus_property.r
val drive_connection_interface : t -> string OBus_property.r
val drive_write_cache : t -> string OBus_property.r
val drive_rotation_rate : t -> int OBus_property.r
val drive_wwn : t -> string OBus_property.r
val drive_serial : t -> string OBus_property.r
val drive_revision : t -> string OBus_property.r
val drive_model : t -> string OBus_property.r
val drive_vendor : t -> string OBus_property.r
val partition_table_count : t -> int OBus_property.r
val partition_table_scheme : t -> string OBus_property.r
val partition_alignment_offset : t -> int64 OBus_property.r
val partition_size : t -> int64 OBus_property.r
val partition_offset : t -> int64 OBus_property.r
val partition_number : t -> int OBus_property.r
val partition_flags : t -> string list OBus_property.r
val partition_uuid : t -> string OBus_property.r
val partition_label : t -> string OBus_property.r
val partition_type : t -> string OBus_property.r
val partition_scheme : t -> string OBus_property.r
val partition_slave : t -> t OBus_property.r
val luks_cleartext_unlocked_by_uid : t -> int OBus_property.r
val luks_cleartext_slave : t -> t OBus_property.r
val luks_holder : t -> t OBus_property.r
val id_label : t -> string OBus_property.r
val id_uuid : t -> string OBus_property.r
val id_version : t -> string OBus_property.r
val id_type : t -> string OBus_property.r
val id_usage : t -> string OBus_property.r
val job_percentage : t -> float OBus_property.r
val job_is_cancellable : t -> bool OBus_property.r
val job_initiated_by_uid : t -> int OBus_property.r
val job_id : t -> string OBus_property.r
val job_in_progress : t -> bool OBus_property.r
val device_presentation_icon_name : t -> string OBus_property.r
val device_presentation_name : t -> string OBus_property.r
val device_presentation_nopolicy : t -> bool OBus_property.r
val device_presentation_hide : t -> bool OBus_property.r
val device_block_size : t -> int64 OBus_property.r
val device_size : t -> int64 OBus_property.r
val device_is_linux_dmmp : t -> bool OBus_property.r
val device_is_linux_dmmp_component : t -> bool OBus_property.r
val device_is_linux_lvm2_pv : t -> bool OBus_property.r
val device_is_linux_lvm2_lv : t -> bool OBus_property.r
val device_is_linux_md : t -> bool OBus_property.r
val device_is_linux_md_component : t -> bool OBus_property.r
val device_is_luks_cleartext : t -> bool OBus_property.r
val device_is_luks : t -> bool OBus_property.r
val device_mounted_by_uid : t -> int OBus_property.r
val device_mount_paths : t -> string list OBus_property.r
val device_is_mounted : t -> bool OBus_property.r
val device_is_optical_disc : t -> bool OBus_property.r
val device_is_drive : t -> bool OBus_property.r
val device_is_read_only : t -> bool OBus_property.r
val device_is_media_change_detection_inhibited : t -> bool OBus_property.r
val device_is_media_change_detection_inhibitable : t -> bool OBus_property.r
val device_is_media_change_detection_polling : t -> bool OBus_property.r
val device_is_media_change_detected : t -> bool OBus_property.r
val device_is_media_available : t -> bool OBus_property.r
val device_is_removable : t -> bool OBus_property.r
val device_is_partition_table : t -> bool OBus_property.r
val device_is_partition : t -> bool OBus_property.r
val device_is_system_internal : t -> bool OBus_property.r
val device_file_by_path : t -> string list OBus_property.r
val device_file_by_id : t -> string list OBus_property.r
val device_file_presentation : t -> string OBus_property.r
val device_file : t -> string OBus_property.r
val device_minor : t -> int64 OBus_property.r
val device_major : t -> int64 OBus_property.r
val device_media_detection_time : t -> int64 OBus_property.r
val device_detection_time : t -> int64 OBus_property.r
val native_path : t -> string OBus_property.r

val properties : t -> OBus_property.group
