(*
 * oBus_property.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Manipulation of DBus object properties *)

type access = [ `readable | `writable ]

val rd_only : [ `readable ]
val wr_only : [ `writable ]
val rdwr : [ `readable | `writable ]
  (** Access modes *)

type ('a, 'access) t
  (** A property of type ['a] with allowed operations ['access]. *)

val make :  interface:OBus_name.interface -> member:OBus_name.member ->
  access:([< access ] as 'access) -> ('a, _) OBus_type.cl_single -> (unit -> OBus_proxy.t Lwt.t) -> ('a, 'access) t
  (** Create a property *)

val dmake :  interface:OBus_name.interface -> member:OBus_name.member ->
  access:([< access ] as 'access) -> (unit -> OBus_proxy.t Lwt.t) -> (OBus_value.single, 'access) t
  (** Create a dynamically typed property *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Set the value of a property *)

val get : ('a, [> `readable ]) t -> 'a Lwt.t
  (** Get the value of a property.

      @rase OBus_type.Cast_failure if the property do not have the
      expected type *)

val get_all : OBus_proxy.t -> OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
  (** Retreive all properties of an object *)
