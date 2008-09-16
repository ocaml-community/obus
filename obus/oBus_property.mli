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

(** Property with known type *)

type ('a, 'access) t

val make : connection:OBus_connection.t -> ?destination:string -> path:OBus_path.t ->
  interface:string -> name:string -> access:([< access ] as 'access) ->
  [< 'a OBus_type.cl_single ] -> ('a, 'access) t
   (** Create a property *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Set the value of a property *)

val get : ('a, [> `readeable]) t -> 'a Lwt.t
  (** Get the value of a property *)

(** Dynamically-typed properties *)

type 'access dt

val dmake : connection:OBus_connection.t -> ?destination:string -> path:OBus_path.t ->
  interface:string -> name:string -> access:([< access ] as 'access) -> 'access dt

val dset : [> `writable ] dt -> OBus_value.single -> unit Lwt.t
val dget : [> `readable ] dt -> OBus_value.single Lwt.t

val dget_all : connection:OBus_connection.t -> ?destination:string -> path:OBus_path.t ->
  interface:string -> (string * OBus_value.single) list Lwt.t
  (** Retreive all properties of an object *)

(**/**)
val lmake : connection:OBus_connection.t Lwt.t Lazy.t -> ?destination:string -> path:OBus_path.t ->
  interface:string -> name:string -> access:([< access ] as 'access) -> [< 'a OBus_type.cl_single ] -> ('a, 'access) t
val ldmake : connection:OBus_connection.t Lwt.t Lazy.t -> ?destination:string -> path:OBus_path.t ->
  interface:string -> name:string -> access:([< access ] as 'access) -> 'access dt
