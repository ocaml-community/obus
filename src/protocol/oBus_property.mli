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

type map = (OBus_context.t * OBus_value.V.single) Map.Make(String).t
  (** Type of all properties of an interface. *)

type group
  (** Type of a group of properties. Property groups are used to
      read/monitor all the properties of an interface. *)

type monitor = OBus_proxy.t -> OBus_name.interface -> Lwt_switch.t -> map React.signal Lwt.t
  (** Type of a function creating a signal holding the contents of all
      the properties of an interface. The default monitor uses the
      [org.freedesktop.DBus.Properties.PropertiesChanged] signal. *)

(** {6 Properties creation} *)

val make : ?monitor : monitor -> ('a, 'access) OBus_member.Property.t -> OBus_proxy.t -> ('a, 'access) t
  (** [make ?monitor property proxy] returns the property object for
      this proxy. *)

val group : ?monitor : monitor -> OBus_proxy.t -> OBus_name.interface -> group
  (** [group ?monitor proxy interface] creates a group for all
      readable properties of the given interface. Note that it is
      faster to read a group of properties rather than reading each
      property individually. *)

(** {6 Properties transformation} *)

val map_rw : ('a -> 'b) -> ('b -> 'a) -> 'a rw -> 'b rw
  (** [map property f g] maps [property] with [f] and [g] *)

val map_rw_with_context : (OBus_context.t -> 'a -> 'b) -> ('b -> 'a) -> 'a rw -> 'b rw
  (** Same as {!map} except that the context is also passed to mapping
      functions. *)

val map_r : ('a -> 'b) -> ('a, [> `readable ]) t -> 'b r
  (** Maps a read-only property. *)

val map_r_with_context :  (OBus_context.t -> 'a -> 'b) -> ('a, [> `readable ]) t -> 'b r
  (** Maps a read-only property, passing the context to the mapping
      function *)

val map_w : ('b -> 'a) -> ('a, [> `writable ]) t -> 'b w
  (** Maps a write-only property. *)

(** {6 Operations on properties} *)

val get : ('a, [> `readable ]) t -> 'a Lwt.t
  (** Read the contents of a property. *)

val get_with_context : ('a, [> `readable ]) t -> (OBus_context.t * 'a) Lwt.t
  (** Same as {!get} but also returns the context *)

val set : ('a, [> `writable ]) t -> 'a -> unit Lwt.t
  (** Write the contents of a property *)

val get_group : group -> map Lwt.t
  (** Returns the set of all properties that belong to the given
      group. *)

(** {6 Operations on property maps} *)

val find_value : OBus_name.member -> map -> OBus_value.V.single
  (** [find_value name map] returns the value associated to [name] in
      [set]. It raises [Not_found] if [name] is not in [map]. *)

val find_value_with_context : OBus_name.member -> map -> OBus_context.t * OBus_value.V.single
  (** Same as {!find_value} but also returns the context in which the
      property was received. *)

val find : ('a, [> `readable ]) t -> map -> 'a
  (** [find property map] looks up for the given property in [set] and
      maps it to a value of type ['a]. It raises [Not_found] if
      [property] does not belong to [map]. *)

val find_with_context : ('a, [> `readable ]) t -> map -> OBus_context.t * 'a
  (** Same as {!find} but also returns the context in which the
      property was received. *)

val print_map : Format.formatter -> map -> unit
  (** [print_set pp map] prints all the properties of [map]. *)

val string_of_map : map -> string
  (** [string_of_set set] prints [set] into a string and returns it. *)

(** {6 Monitoring} *)

(** Lots of D-Bus services notify other applications with a D-Bus
    signal when one or more properties of an object change. In this
    case it is possible to monitor the contents of a property.

    Note that when at least one property of an interface is monitored,
    obus will keep a local state of all the properties of the
    interface.
*)

val monitor : ?switch : Lwt_switch.t -> ('a, [> `readable ]) t -> 'a React.signal Lwt.t
  (** [monitor ?switch property] returns the signal holding the
      current contents of [property]. Raises [Failure] if the property
      is not monitorable.

      Resources allocated to monitor the property are automatically
      freed when the signal is garbage collected *)

val monitor_group : ?switch : Lwt_switch.t -> group -> map React.signal Lwt.t
  (** [monitor_group ?switch group] monitors all properties of the
      given group. *)

(** {6 Helpers for custom monitors} *)

val get_all_no_cache : OBus_proxy.t -> OBus_name.interface -> (OBus_context.t * (OBus_name.member * OBus_value.V.single) list) Lwt.t
  (** [get_all_no_cache proxy interface] reads the value of all
      properties without using the cache. *)

val update_map : OBus_context.t -> (OBus_name.member * OBus_value.V.single) list -> map -> map
  (** [update_map context values map] add all properties with their
      context and value to [map]. *)

val map_of_list : OBus_context.t -> (OBus_name.member * OBus_value.V.single) list -> map
  (** [map_of_list context values] returns the map corresponding to
      the given values and context. *)
