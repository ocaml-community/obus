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

type notify_mode
  (** Type of properties changes notification. It describes how
      properties changes are announced. *)

(** {6 Proxy properties creation} *)

val make : ('a, 'access) OBus_member.Property.t -> ?notify_mode : notify_mode -> OBus_proxy.t -> ('a, 'access) t
  (** [make property ?notify_mode proxy] returns the property object
      for this proxy. [notify_mode] defaults to {!notify_none} *)

(** {6 Properties transformation} *)

val map_rw : ('a -> 'b) -> ('b -> 'a) -> 'a rw -> 'b rw
  (** [map property f g] maps [property] with [f] and [g] *)

val map_rw_with_context : (unit OBus_context.t -> 'a -> 'b) -> ('b -> 'a) -> 'a rw -> 'b rw
  (** Same as {!map} except that the context is also passed to mapping
      functions. *)

val map_r : ('a -> 'b) -> ('a, [> `readable ]) t -> 'b r
  (** Maps a read-only properties *)

val map_r_with_context :  (unit OBus_context.t -> 'a -> 'b) -> ('a, [> `readable ]) t -> 'b r
  (** Maps a read-only properties, passing the context to the mapping
      function *)

val map_w : ('b -> 'a) -> ('a, [> `writable ]) t -> 'b w
  (** Maps a write-only properties *)

(** {6 Operation on properties} *)

val get : ?cache : bool -> ('a, [> `readable ]) t -> 'a Lwt.t
  (** Read the contents of a property.

      If [cache] is [true] (the default) and the given property is not
      cached, then obus automatically fill the cache. The cache is
      filled using the "org.freedesktop.DBus.Properties.GetAll"
      methods.

      Note that the cache will expire at the next iteration of the Lwt main
      loop. *)

val get_with_context : ?cache : bool -> ('a, [> `readable ]) t -> (unit OBus_context.t * 'a) Lwt.t
  (** Same as {!get} but also returns the context *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Write the contents of a property *)

(** {6 Monitoring} *)

(** Lots of D-Bus services notify other applications with a D-Bus
    signal when one or more properties of an object change. In this
    case it is possible to monitor the contents of a property.

    Note that when at least one property of an interface is monitored,
    obus will keep a local state of all the properties of the
    interface, until all signals (see {!monitor}) are garbage
    collected, or stopped with {!unmonitor}.
*)

val monitorable : ('a, [> `readable ]) t -> bool
  (** Returns whether the given proeprty can be monitored *)

val monitor : ('a, [> `readable ]) t -> 'a React.signal Lwt.t
  (** [monitor property] returns the signal holding the current
      contents of [property]. Raises [Failure] if the property is not
      monitorable.  *)

val unmonitor : ('a, [> `readable ]) t -> unit
  (** Stop monitoring the property. If it was not monitored, it does
      nothing. *)

(** {6 Properties changes notifications} *)

val notify_none : notify_mode
  (** Property change are not announced *)

val notify_global : OBus_name.member -> notify_mode
  (** [notify_global name] means that when one or more properties of
      the interface changes, the signal with name [name] is emited,
      but it does not contain update informations. In this case the
      ["GetAll"] method of the object is invoked to get new
      properties.

      Such a signal is generally called ["Changed"]. It is used for
      example by [udisks] or [upower].  *)

val notify_update : OBus_name.member -> notify_mode
  (** [notify_update name] is the same as [notify_global name] except
      that the signal carries properties updates (with D-Bus type
      ["a{sv}"]). In this case it is not necessary to call ["GetAll"].

      Such a signal is generally called ["PropertiesChanged"].
      It is used for example by [network-manager]. *)

val notify_egg_dbus : notify_mode
  (** EggDBus notification mode. It is used by services using the
      EggDBus library. *)

type 'a name_map = 'a Map.Make(String).t

type notify_data = (unit OBus_context.t * OBus_value.V.single) name_map
    (** Data that must be returned by notifiers. It is a mapping from
        property names to their value and the context in which they
        were received. *)

(** Type of a notifier *)
type notifier = {
  notifier_signal : notify_data React.signal;
  (** Signal holding the current value of all properties and the
      context in which they were received *)

  notifier_stop : unit -> unit;
  (** [stop ()] cleans up allocated resources when no properties are
      monitored *)
}

val notify_custom : (OBus_proxy.t -> OBus_name.interface -> notifier Lwt.t) -> notify_mode
  (** [notify_custom f] represents a cusom mode for being notified of
      property changes. [f] is the function used to create the
      notifier. *)

val get_all_no_cache : OBus_proxy.t -> OBus_name.interface -> notify_data Lwt.t
  (** [get_all_no_cache connection owner path interface] returns the
      values of all properties of the given object for the given
      interface.

      Contrary to {!get_all}, {!get_all_no_cache} does not use cached
      values, it always send a new request. {!get_all_no_cache} is
      meant to be used with {!notify_custom}. *)

(** {6 Receving all properties} *)

val get_all : OBus_proxy.t -> interface : OBus_name.interface -> OBus_value.V.single name_map Lwt.t
  (** [get_all proxy ~interface ()] returns all
      properties of the givne object with their values.

      Note that {!get_all} always uses the cache if it is not empty,
      or fills it if it is. *)

val get_all_with_context : OBus_proxy.t -> interface : OBus_name.interface -> (unit OBus_context.t * OBus_value.V.single) name_map Lwt.t
  (** Same as {!get_all} but also returns the context *)
