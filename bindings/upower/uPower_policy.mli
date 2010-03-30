(*
 * uPower_policy.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Quality of service policy *)

type cookie
  (** Type of request identifiers *)

type request = [ `Cpu_dma | `Network ]
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

  lr_typ : request;
  (** The type of the request.*)

  lr_reserved : string;

  lr_value : int;
  (** The value, in microseconds or kilobits per second. *)
}

val get_latency_requests : UPower.t -> latency_request list Lwt.t
  (** Gets all the system requests from all services and applications. *)

val get_latency : UPower.t -> string -> int Lwt.t
val cancel_request : UPower.t -> request -> cookie -> unit Lwt.t
val request_latency : UPower.t -> request -> int -> bool -> cookie Lwt.t
val set_minimum_latency : UPower.t -> string -> int -> unit Lwt.t

val requests_changed : UPower.t -> unit OBus_signal.t
val latency_changed : UPower.t -> (string * bool) OBus_signal.t
