(*
 * nm_settings.mli
 * ---------------
 * Copyright : (c) 2010, Pierre Chambart <chambart@crans.org>
 *                 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** NetworkManager settings *)

include OBus_proxy.Private

val user : unit -> t Lwt.t
(** [user ()] returns the proxy object for user settings. The object
      is on the session message bus. *)

val system : unit -> t Lwt.t
(** [system ()] returns the proxy object for system settings. The
      object is on the system message bus *)

(** Connection settings *)
module Connection : sig
  include OBus_proxy.Private

  (** {6 Methods} *)

  val update :
    t ->
    properties:(string * (string * OBus_value.V.single) list) list ->
    unit Lwt.t

  val delete : t -> unit Lwt.t

  val get_settings :
    t -> (string * (string * OBus_value.V.single) list) list Lwt.t

  (** {6 Signals} *)

  val updated :
    t -> (string * (string * OBus_value.V.single) list) list OBus_signal.t

  val removed : t -> unit OBus_signal.t

  module Secrets : sig
    val get_secrets :
      t ->
      setting_name:string ->
      hints:string list ->
      request_new:bool ->
      (string * (string * OBus_value.V.single) list) list Lwt.t
  end
end

(** System settings *)
module System : sig
  val save_hostname : t -> hostname:string -> unit Lwt.t

  val hostname : t -> string OBus_property.r

  val can_modify : t -> bool OBus_property.r

  val properties_changed :
    t -> (string * OBus_value.V.single) list OBus_signal.t

  val check_permissions : t -> unit OBus_signal.t

  val get_permissions : t -> int Lwt.t
end

(** {6 Methods} *)

val list_connections : t -> Connection.t list Lwt.t

(** {6 Signals} *)

val add_connection :
  t ->
  connection:(string * (string * OBus_value.V.single) list) list ->
  unit Lwt.t

val new_connection : t -> Connection.t OBus_signal.t
