(*
 * oBus_property.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus properties *)

(** This module offers a convenient way to deal with D-Bus
    properties. It allows to read/write the contents of a property,
    and, when possible, to monitor it. *)

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

type 'a access
  (** Type of access modes *)

val readable : [ `readable ] access
  (** Access mode for readable properties *)

val writable : [ `writable ] access
  (** Access mode for writable properties *)

val readable_writable : [ `readable | `writable ] access
  (** Access mode for readable and writable properties *)

(** {6 Operation on properties} *)

val get : ('a, [> `readable ]) t -> 'a Lwt.t
  (** Read the contents of a property *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Write the contents of a property *)

(** {6 Monitoring} *)

(** Lots of D-Bus services notifies other applications with a D-Bus
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

type notify_mode
  (** Type of properties changes notification. It describes how
      properties changes are announced. *)

val notify_none : notify_mode
  (** Properties changes are not announced. {!monitor} will fail in
      this case. *)

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

type notify_data = (OBus_type.context * OBus_value.single) Map.Make(String).t
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

val notify_custom : (OBus_connection.t -> OBus_name.bus option -> OBus_path.t -> OBus_name.interface -> notifier Lwt.t) -> notify_mode
  (** [notify_custom f] represent a cusom mode for being notified of
      property changes. [f] is the function used to create the
      notifier. *)

val get_all_no_cache : OBus_connection.t -> OBus_name.bus option -> OBus_path.t -> OBus_name.interface -> notify_data Lwt.t
  (** [get_all_no_cache connection owner path interface] returns the
      values of all properties of the given object for the given
      interface.

      Contrary to {!get_all}, {!get_all_no_cache} does not use cached
      values, it always send a new request. {!get_all_no_cache} is
      meant to be used with {!notify_custom}. *)

(** {6 Property creation} *)

val make :
  connection : OBus_connection.t ->
  ?owner : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  access : 'access access ->
  ?notify : notify_mode ->
  ('a, _) OBus_type.cl_single -> ('a, 'access) t
  (** [make ~connection ?sender ~path ~interface ~member ~access
      ?changed typ] creates a property with the given interface and
      member.

      @param owner name of the peer owning the property.

      @param notify indicates how properties changes are announced. It
      defaults to [notify_none]. *)

val dyn_make :
  connection : OBus_connection.t ->
  ?owner : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  access : 'access access ->
  ?notify : notify_mode ->
  unit -> (OBus_value.single, 'access) t
  (** Same as {!make} but using dynamically typed values *)

(** {6 Receving all properties} *)

val get_all :
  connection : OBus_connection.t ->
  ?owner : OBus_name.bus ->
  path : OBus_path.t ->
  interface : OBus_name.interface ->
  unit -> OBus_value.single Map.Make(String).t Lwt.t
  (** [get_all ~connection ?owner ~path ~interface ()] returns all
      properties of the givne object with their values.  If these
      values are available in the cache (because one or more
      properties are monitored), then cached values are returned. *)
