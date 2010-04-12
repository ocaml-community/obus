(*
 * uPower_policy.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Quality of service policy *)

(** {6 Types} *)

type cookie
  (** Type of request identifiers *)

type latency = [ `Cpu_dma | `Network ]
  (** Type of latency request *)

type latency_request = {
  lr_cookie : cookie;
  (** The random cookie that identifies the request. *)

  lr_uid : int;
  (** The user ID that issued the request. *)

  lr_pid : int;
  (** The process ID of the application. *)

  lr_exec : string;
  (** The executable that issued the request. *)

  lr_timespec : int64;
  (** The number of seconds since the epoch. *)

  lr_persistent : bool;
  (** If the request is persistent and outlives the connection lifetime. *)

  lr_typ : latency;
  (** The type of the request.*)

  lr_reserved : string;

  lr_value : int;
  (** The value, in microseconds or kilobits per second. *)
}

(** {6 Methods} *)

val get_latency_requests : UPower.t -> latency_request list Lwt.t
val get_latency : UPower.t -> latency : latency -> int Lwt.t

val request_latency : UPower.t -> latency : latency -> value : int -> persistent : bool -> cookie Lwt.t
val cancel_request : UPower.t -> latency : latency -> cookie : cookie -> unit Lwt.t

val set_minimum_latency : UPower.t -> latency : latency -> value : int -> unit Lwt.t

(** {6 Signals} *)

val requests_changed : UPower.t -> unit OBus_signal.t
val latency_changed : UPower.t -> (latency * int) OBus_signal.t
