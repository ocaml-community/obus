(*
 * uDisks.mli
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** UDisks main interface *)

include OBus_peer.Private

val daemon : unit -> t Lwt.t
  (** [daemon ()] returns the peer object for the udisks daemon *)

(** {6 Exceptions} *)

val busy : OBus_error.name
val cancelled : OBus_error.name
val failed : OBus_error.name
val filesystem_driver_missing : OBus_error.name
val filesystem_tools_missing : OBus_error.name
val inhibited : OBus_error.name
val invalid_option : OBus_error.name
val not_found : OBus_error.name
val not_supported : OBus_error.name
val permission_denied : OBus_error.name

(** {6 Methods} *)

type inhibit_cookie

val inhibit : t -> inhibit_cookie Lwt.t
val uninhibit : t -> cookie : inhibit_cookie -> unit Lwt.t

val linux_md_create : t ->
  components : UDisks_device.t list ->
  level : string ->
  stripe_size : int64 ->
  name : string ->
  options : string list ->
  UDisks_device.t Lwt.t
val linux_md_start : t ->
  components : UDisks_device.t list ->
  options : string list ->
  UDisks_device.t Lwt.t

val linux_lvm2_lvcreate : t ->
  group_uuid : string ->
  name : string ->
  size : int64 ->
  num_stripes : int ->
  stripe_size : int64 ->
  num_mirrors : int ->
  options : string list ->
  fstype : string ->
  fsoptions : string list ->
  UDisks_device.t Lwt.t
val linux_lvm2_lvremove : t -> group_uuid : string -> uuid : string -> options : string list -> unit Lwt.t
val linux_lvm2_lvstart : t -> group_uuid : string -> uuid : string -> options : string list -> unit Lwt.t
val linux_lvm2_lvset_name : t -> group_uuid : string -> uuid : string -> name : string -> unit Lwt.t
val linux_lvm2_vgremove_pv : t -> vg_uuid : string -> pv_uuid : string -> options : string list -> unit Lwt.t
val linux_lvm2_vgadd_pv : t -> uuid : string -> physical_volume : UDisks_device.t -> options : string list -> unit Lwt.t
val linux_lvm2_vgset_name : t -> uuid : string -> name : string -> unit Lwt.t
val linux_lvm2_vgstop : t -> uuid : string -> options : string list -> unit Lwt.t
val linux_lvm2_vgstart : t -> uuid : string -> options : string list -> unit Lwt.t

type all_spindown_timeouts_cookie

val drive_set_all_spindown_timeouts : t -> timeout_seconds : int -> options : string list -> all_spindown_timeouts_cookie Lwt.t
val drive_unset_all_spindown_timeouts : t -> cookie : all_spindown_timeouts_cookie -> unit Lwt.t

type inhibit_all_polling_cookie

val drive_inhibit_all_polling : t -> options : string list -> inhibit_all_polling_cookie Lwt.t
val drive_uninhibit_all_polling : t -> cookie : inhibit_all_polling_cookie -> unit Lwt.t

val find_device_by_major_minor : t -> device_major : int64 -> device_minor : int64 -> UDisks_device.t Lwt.t
val find_device_by_device_file : t -> device_file : string -> UDisks_device.t Lwt.t

val enumerate_device_files : t -> string list Lwt.t
val enumerate_devices : t -> UDisks_device.t list Lwt.t
val enumerate_ports : t -> UDisks_port.t list Lwt.t
val enumerate_expanders : t -> UDisks_expander.t list Lwt.t
val enumerate_adapters : t -> UDisks_adapter.t list Lwt.t

(** {6 Signals} *)

val port_changed : t -> UDisks_port.t OBus_signal.t
val port_removed : t -> UDisks_port.t OBus_signal.t
val port_added : t -> UDisks_port.t OBus_signal.t

val expander_changed : t -> UDisks_expander.t OBus_signal.t
val expander_removed : t -> UDisks_expander.t OBus_signal.t
val expander_added : t -> UDisks_expander.t OBus_signal.t

val adapter_changed : t -> UDisks_adapter.t OBus_signal.t
val adapter_removed : t -> UDisks_adapter.t OBus_signal.t
val adapter_added : t -> UDisks_adapter.t OBus_signal.t

val device_changed : t -> UDisks_device.t OBus_signal.t
val device_removed : t -> UDisks_device.t OBus_signal.t
val device_added : t -> UDisks_device.t OBus_signal.t

type job = {
  job_device : UDisks_device.t;
  job_in_progress : bool;
  (** Whether a job is currently in progress.</doc:summary *)
  job_is_cancellable : bool;
  (** Whether the job is cancellable *)
  job_id : string;
  (** The identifier of the job *)
  job_num_tasks : int;
  (** Number of tasks in the job *)
  job_cur_task : int;
  (** Current task number (zero-based offset) *)
  job_cur_task_id : string;
  (** Task identifier for current task *)
  job_cur_task_percentage : float;
  (** Percentage completed of current task (between 0 and 100, negative if unknown) *)
}

val device_job_changed : t -> job OBus_signal.t

(** {6 Properties} *)

(** File-system informations *)
type fs = {
  fs_id : string;
  (** The name / identifier of the file system (such as ext3 or vfat),
      similar to the contents of the Device:IdType property. *)
  fs_name : string;
  (** A human readable name for the file system such as "Linux
      Ext3". *)
  fs_supports_unix_owners : bool;
  (** Whether the file system supports the UNIX owners model
      (e.g. ext3 does, but vfat doesn't). *)
  fs_can_mount : bool;
  (** Whether the file system can be mounted. *)
  fs_can_create : bool;
  (** Whether the file system can be created on a device. *)
  fs_max_label_len : int;
  (** The maximum amount of bytes that the file system label can
      hold. Set to zero if the file system doesn't support labels. *)
  fs_supports_label_rename : bool;
  (** Whether the label of the file system can be changed. *)
  fs_supports_online_label_rename : bool;
  (** Whether the label can be changed while the file system is
      mounted. *)
  fs_supports_fsck : bool;
  (** Whether the file system can be checked. *)
  fs_supports_online_fsck : bool;
  (** Whether the file system can be checked while mounted. *)
  fs_supports_resize_enlarge : bool;
  (** Whether the file system can be enlarged. *)
  fs_supports_online_resize_enlarge : bool;
  (** Whether the file system can be enlarged while mounted. *)
  fs_supports_resize_shrink : bool;
  (** Whether the file system can be shrunk. *)
  fs_supports_online_resize_shrink : bool;
  (** Whether the file system can be shrunk while mounted. *)
}

val known_filesystems : t -> fs list OBus_property.r
val supports_luks_devices : t -> bool OBus_property.r
val daemon_is_inhibited : t -> bool OBus_property.r
val daemon_version : t -> string OBus_property.r

type properties = {
  known_filesystems : fs list;
  supports_luks_devices : bool;
  daemon_is_inhibited : bool;
  daemon_version : string;
}

val properties : t -> properties OBus_property.r
