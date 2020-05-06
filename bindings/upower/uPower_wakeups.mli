(*
 * uPower_wakeups.mli
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** UPower wakeups interface *)

(** {6 Types} *)

type data = {
  data_is_userspace : bool;  (** If the wakeup is from userspace ? *)
  data_id : int;
      (** The process ID of the application, or the IRQ for kernel
      drivers. *)
  data_value : float;  (** The number of wakeups per second. *)
  data_cmdline : string option;
      (** The command line for the application, or [None] for kernel
      drivers. *)
  data_details : string;  (** The details about the wakeup. *)
}
(** The data of all the processes and drivers which contribute to the
    wakeups on the system. *)

(** {6 Methods} *)

val get_data : UPower.t -> data list Lwt.t

val get_total : UPower.t -> int Lwt.t

(** {6 Signals} *)

val data_changed : UPower.t -> unit OBus_signal.t

val total_changed : UPower.t -> int OBus_signal.t

(** {6 Properties} *)

val has_capability : UPower.t -> bool OBus_property.r
