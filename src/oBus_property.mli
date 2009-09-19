(*
 * oBus_property.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of DBus object properties *)

type 'mode access

val rd_only : [ `readable ] access
val wr_only : [ `writable ] access
val rdwr : [ `readable | `writable ] access
  (** Access modes *)

type ('a, 'mode) t
  (** A property of type ['a] with allowed operations ['mode]. *)

val make :  interface:OBus_name.interface -> member:OBus_name.member ->
  access:'mode access -> ('a, _) OBus_type.cl_single -> OBus_proxy.t -> ('a, 'mode) t
  (** Create a property *)

val dyn_make : interface:OBus_name.interface -> member:OBus_name.member ->
  access:'mode access -> OBus_proxy.t -> (OBus_value.single, 'mode) t
  (** Create a dynamically typed property *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Set the value of a property *)

val get : ('a, [> `readable ]) t -> 'a Lwt.t
  (** Get the value of a property.

      @rase OBus_type.Cast_failure if the property do not have the
      expected type *)

val get_all : OBus_proxy.t -> OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
  (** Retreive all properties of an object *)

(**/**)

val make_custom :  interface:OBus_name.interface -> member:OBus_name.member ->
  access:'mode access -> ('a, _) OBus_type.cl_single -> (unit -> OBus_proxy.t Lwt.t) -> ('a, 'mode) t

val dyn_make_custom : interface:OBus_name.interface -> member:OBus_name.member ->
  access:'mode access -> (unit -> OBus_proxy.t Lwt.t) -> (OBus_value.single, 'mode) t
