(*
 * avahi.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* This a start of avahi binding. It need to be higly completed by
   creating more suitable types. *)

module Address_resolver : sig
  type t
  val free : t -> unit Lwt.t
  val on_found : t -> (int -> int -> int -> string -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_failure : t -> (string -> unit) -> OBus_signal.receiver Lwt.t
end

module Domain_browser : sig
  type t
  val free : t -> unit Lwt.t
  val on_item_new : t -> (int -> int -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_item_remove : t -> (int -> int -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_failure : t -> (string -> unit) -> OBus_signal.receiver Lwt.t
  val on_all_for_now : t -> (unit -> unit) -> OBus_signal.receiver Lwt.t
  val on_cache_exhausted : t -> (unit -> unit) -> OBus_signal.receiver Lwt.t
end

module Entry_group : sig
  type t
  val free : t -> unit Lwt.t
  val commit : t -> unit Lwt.t
  val reset : t -> unit Lwt.t
  val get_state : t -> int Lwt.t
  val on_state_changed : t -> (int -> string -> unit) -> OBus_signal.receiver Lwt.t
  val is_empty : t -> bool Lwt.t
  val add_service : t -> int -> int -> int -> string -> string -> string -> string -> int -> char list list -> unit Lwt.t
  val add_service_subtype : t -> int -> int -> int -> string -> string -> string -> string -> unit Lwt.t
  val update_service_txt : t -> int -> int -> int -> string -> string -> string -> char list list -> unit Lwt.t
  val add_address : t -> int -> int -> int -> string -> string -> unit Lwt.t
  val add_record : t -> int -> int -> int -> string -> int -> int -> int -> char list -> unit Lwt.t
end

module Host_name_resolver : sig
  type t
  val free : t -> unit Lwt.t
  val on_found : t -> (int -> int -> string -> int -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_failure : t -> (string -> unit) -> OBus_signal.receiver Lwt.t
end

module Record_browser : sig
  type t
  val free : t -> unit Lwt.t
  val on_item_new : t -> (int -> int -> string -> int -> int -> char list -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_item_remove : t -> (int -> int -> string -> int -> int -> char list -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_failure : t -> (string -> unit) -> OBus_signal.receiver Lwt.t
  val on_all_for_now : t -> (unit -> unit) -> OBus_signal.receiver Lwt.t
  val on_cache_exhausted : t -> (unit -> unit) -> OBus_signal.receiver Lwt.t
end

module Service_browser : sig
  type t
  val free : t -> unit Lwt.t
  val on_item_new : t -> (int -> int -> string -> string -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_item_remove : t -> (int -> int -> string -> string -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_failure : t -> (string -> unit) -> OBus_signal.receiver Lwt.t
  val on_all_for_now : t -> (unit -> unit) -> OBus_signal.receiver Lwt.t
  val on_cache_exhausted : t -> (unit -> unit) -> OBus_signal.receiver Lwt.t
end

module Service_resolver : sig
  type t
  val free : t -> unit Lwt.t
  val on_found : t -> (int -> int -> string -> string -> string -> string -> int -> string -> int -> char list list -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_failure : t -> (string -> unit) -> OBus_signal.receiver Lwt.t
end

module Service_type_browser : sig
  type t
  val free : t -> unit Lwt.t
  val on_item_new : t -> (int -> int -> string -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_item_remove : t -> (int -> int -> string -> string -> int -> unit) -> OBus_signal.receiver Lwt.t
  val on_failure : t -> (string -> unit) -> OBus_signal.receiver Lwt.t
  val on_all_for_now : t -> (unit -> unit) -> OBus_signal.receiver Lwt.t
  val on_cache_exhausted : t -> (unit -> unit) -> OBus_signal.receiver Lwt.t
end

module Server : sig
  val get_version_string : unit -> string Lwt.t
  val get_apiversion : unit -> int Lwt.t
  val get_host_name : unit -> string Lwt.t
  val set_host_name : string -> unit Lwt.t
  val get_host_name_fqdn : unit -> string Lwt.t
  val get_domain_name : unit -> string Lwt.t
  val is_nsssupport_available : unit -> bool Lwt.t
  val get_state : unit -> int Lwt.t
  val on_state_changed : (int -> string -> unit) -> OBus_signal.receiver Lwt.t
  val get_local_service_cookie : unit -> int Lwt.t
  val get_alternative_host_name : string -> string Lwt.t
  val get_alternative_service_name : string -> string Lwt.t
  val get_network_interface_name_by_index : int -> string Lwt.t
  val get_network_interface_index_by_name : string -> int Lwt.t

  val resolve_host_name : int -> int -> string -> int -> int -> (int * int * string * int * string * int) Lwt.t
  val resolve_address : int -> int -> string -> int -> (int * int * int * string * string * int) Lwt.t
  val resolve_service : int -> int -> string -> string -> string -> int -> int -> (int * int * string * string * string * string * int * string * int * char list list * int) Lwt.t

  val entry_group_new : unit -> Entry_group.t Lwt.t
  val domain_browser_new : int -> int -> string -> int -> int -> Domain_browser.t Lwt.t
  val service_type_browser_new : int -> int -> string -> int -> Service_type_browser.t Lwt.t
  val service_browser_new : int -> int -> string -> string -> int -> Service_browser.t Lwt.t
  val service_resolver_new : int -> int -> string -> string -> string -> int -> int -> Service_resolver.t Lwt.t
  val host_name_resolver_new : int -> int -> string -> int -> int -> Host_name_resolver.t Lwt.t
  val address_resolver_new : int -> int -> string -> int -> Address_resolver.t Lwt.t
  val record_browser_new : int -> int -> string -> int -> int -> int -> Record_browser.t Lwt.t
end
