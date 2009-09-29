(*
 * oBus_property.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of DBus object properties *)

val get : OBus_proxy.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, _) OBus_type.cl_single -> 'a Lwt.t
  (** [get proxy ~interface ~member typ] returns the value of the
      given property *)

val set : OBus_proxy.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  ('a, _) OBus_type.cl_single -> 'a -> unit Lwt.t
  (** [set proxy ~interface ~member typ value] sets the value of the
      given property *)

val dyn_get : OBus_proxy.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member -> OBus_value.single Lwt.t
  (** [dyn_get proxy ~interface ~member] returns the value of the
      given property as a dynamically typed value *)

val dyn_set : OBus_proxy.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  OBus_value.single -> unit Lwt.t
  (** [dyn_set proxy ~interface ~member value] sets the value of the
      given property *)

val dyn_get_all : OBus_proxy.t -> interface : OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
  (** [dyn_get_all t ~interface] returns the list of all properties of
      the given proxy with their values *)
