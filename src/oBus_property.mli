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

type properties = OBus_value.V.single Map.Make(String).t
    (** Mapping from property names to their value *)

(** {6 Properties creation} *)

val make : ('a, 'access) OBus_member.Property.t -> ?notify_mode : notify_mode -> OBus_proxy.t -> ('a, 'access) t
  (** [make property ?notify_mode proxy] returns the property object
      for this proxy. [notify_mode] defaults to {!notify_none} *)

val make_group : OBus_proxy.t -> ?notify_mode : notify_mode -> OBus_name.interface -> properties r
  (** [make_group proxy ?notify_mode interface] creates a group of all
      properties of the given interface. *)

(** {6 Properties transformation} *)

val map_rw : ('a -> 'b) -> ('b -> 'a) -> 'a rw -> 'b rw
  (** [map property f g] maps [property] with [f] and [g] *)

val map_rw_with_context : (OBus_context.void OBus_context.t -> 'a -> 'b) -> ('b -> 'a) -> 'a rw -> 'b rw
  (** Same as {!map} except that the context is also passed to mapping
      functions. *)

val map_r : ('a -> 'b) -> ('a, [> `readable ]) t -> 'b r
  (** Maps a read-only properties *)

val map_r_with_context :  (OBus_context.void OBus_context.t -> 'a -> 'b) -> ('a, [> `readable ]) t -> 'b r
  (** Maps a read-only properties, passing the contexxot to the mapping
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

val get_with_context : ?cache : bool -> ('a, [> `readable ]) t -> (OBus_context.void OBus_context.t * 'a) Lwt.t
  (** Same as {!get} but also returns the context *)

val find : ('a, [> `readable ]) t -> OBus_context.void OBus_context.t -> properties -> 'a
  (** [find property context properties] looks up for the given
      property in [properties]. It raises [Not_found] if the
      [property] does not belong to [properties] *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Write the contents of a property *)

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

(** {6 Property changes notifications} *)

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

(** Type of a notifier *)
type notifier = {
  notifier_signal : (OBus_context.void OBus_context.t * properties) React.signal;
  (** Signals holding the contents of all property of an interface *)

  notifier_stop : unit -> unit;
  (** [stop ()] cleans up allocated resources when no properties are
      monitored *)
}

val notify_custom : (OBus_proxy.t -> OBus_name.interface -> notifier Lwt.t) -> notify_mode
  (** [notify_custom f] represents a cusom mode for being notified of
      property changes. [f] is the function used to create the
      notifier. *)

val get_all_no_cache : OBus_proxy.t -> OBus_name.interface -> (OBus_context.void OBus_context.t * properties) Lwt.t
  (** [get_all_no_cache connection owner path interface] returns the
      values of all properties of the given object for the given
      interface.

      Contrary to {!get_all}, {!get_all_no_cache} does not use cached
      values, it always send a new request. {!get_all_no_cache} is
      meant to be used with {!notify_custom}. *)
