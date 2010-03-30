(*
 * oBus_property.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Client-side properties *)

(** This module offers a convenient way to deal with D-Bus
    properties. *)

type ('a, 'access) t
  (** Type of a property holding a value of type ['a]. [access] is the
      access mode of the property. *)

type 'a r = ('a, [ `readable ]) t
  (** Type of read-only properties *)

type 'a w = ('a, [ `writable ]) t
  (** Type of write-only properties *)

type 'a rw = ('a, [ `readable | `writable ]) t
  (** Type of read and write properties *)

(** {6 Operation on properties} *)

val get : ('a, [> `readable ]) t -> 'a Lwt.t
  (** Read the contents of a property *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Write the contents of a property *)

val contents : ('a, [> `readable ]) t -> 'a React.signal Lwt.t
  (** [contents property] returns the signal holding the current
      contents of [property] *)

(** {6 Property creation} *)

type 'a access

val readable : [ `readable ] access
val writable : [ `writable ] access
val readable_writable : [ `readable | `writable ] access

val make :
  connection : OBus_connection.t ->
  ?sender : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  access : 'access access ->
  ?changed : OBus_name.member ->
  ('a, _) OBus_type.cl_single -> ('a, 'access) t
  (** [make ~connection ?sender ~path ~interface ~member typ] creates
      a property for the given interface and member.

      [changed] is the name of a member of the interface which is used
      to notify that one or more properties have changed *)

val dyn_make :
  connection : OBus_connection.t ->
  ?sender : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  access : 'access access ->
  ?changed : OBus_name.member -> unit -> (OBus_value.single, 'access) t
  (** Same as {!make} but using dynamically typed values *)
