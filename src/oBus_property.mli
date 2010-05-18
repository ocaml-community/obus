(*
 * oBus_property.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus properties *)

(** {6 Types} *)

type ('a, 'access) t
  (** Type of a property holding a value of type ['a]. ['access] is
      the access mode of the property. *)

type 'a r = ('a, [ `readable ]) t
    (** Type of read-only properties *)

type 'a w = ('a, [ `writable ]) t
    (** Type of write-only properties *)

type 'a rw = ('a, [ `readable | `writable ]) t
    (** Type of read and write properties *)

type properties = OBus_value.V.single Map.Make(String).t
    (** Mapping from property names to their value *)

(** {6 Properties creation} *)

val make : ('a, 'access) OBus_member.Property.t -> OBus_proxy.t -> ('a, 'access) t
  (** [make property proxy] returns the property object for this
      proxy. *)

val make_group : OBus_proxy.t -> OBus_name.interface -> properties r
  (** [make_group proxy interface] creates a group of all properties
      of the given interface. *)

(** {6 Properties transformation} *)

val map_rw : ('a -> 'b) -> ('b -> 'a) -> 'a rw -> 'b rw
  (** [map property f g] maps [property] with [f] and [g] *)

val map_rw_with_context : (OBus_context.void OBus_context.t -> 'a -> 'b) -> ('b -> 'a) -> 'a rw -> 'b rw
  (** Same as {!map} except that the context is also passed to mapping
      functions. *)

val map_r : ('a -> 'b) -> ('a, [> `readable ]) t -> 'b r
  (** Maps a read-only properties *)

val map_r_with_context :  (OBus_context.void OBus_context.t -> 'a -> 'b) -> ('a, [> `readable ]) t -> 'b r
  (** Maps a read-only properties, passing the con
texxot to the mapping
      function *)

val map_w : ('b -> 'a) -> ('a, [> `writable ]) t -> 'b w
  (** Maps a write-only properties *)

(** {6 Operation on properties} *)

val get : ('a, [> `readable ]) t -> 'a Lwt.t
  (** Read the contents of a property. *)

val get_with_context : ('a, [> `readable ]) t -> (OBus_context.void OBus_context.t * 'a) Lwt.t
  (** Same as {!get} but also returns the context *)

val find : ('a, [> `readable ]) t -> OBus_context.void OBus_context.t -> properties -> 'a
  (** [find property context properties] looks up for the given
      property in [properties]. It raises [Not_found] if the
      [property] does not belong to [properties] *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Write the contents of a property *)

val invalidate : ('a, [> `readable ]) t -> unit
  (** Invalidates the cached value of a property. This make obus to
      refetch the value of the property. *)

(** {6 Monitoring} *)

(** Lots of D-Bus services notify other applications with a D-Bus
    signal when one or more properties of an object change. In this
    case it is possible to monitor the contents of a property.

    Note that when at least one property of an interface is monitored,
    obus will keep a local state of all the properties of the
    interface.
*)

val monitor : ('a, [> `readable ]) t -> 'a React.signal Lwt.t
  (** [monitor property] returns the signal holding the current
      contents of [property]. Raises [Failure] if the property is not
      monitorable.

      Resources allocated to monitor the property are automatically
      freed when the signal is garbage collected *)

val monitor_with_stopper : ('a, [> `readable ]) t -> ('a React.signal * (unit -> unit)) Lwt.t
  (** Same as {!monitor} but also returns a function that can be used
      to explicitly free resources *)

(** {6 Receving all properties} *)

val get_all : OBus_proxy.t -> interface : OBus_name.interface -> properties Lwt.t
  (** [get_all proxy ~interface ()] returns all
      properties of the givne object with their values.

      Note that {!get_all} always uses the cache if it is not empty,
      or fills it if it is. *)

val get_all_with_context : OBus_proxy.t -> interface : OBus_name.interface -> (OBus_context.void OBus_context.t * properties) Lwt.t
  (** Same as {!get_all} but also returns the context *)

val invalidate_all : OBus_proxy.t -> interface : OBus_name.interface -> unit
  (** Invalidates the cached values of all properties of the given
      interface *)
