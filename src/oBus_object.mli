(*
 * oBus_object.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Local D-Bus objects *)

(** This module allows you to create D-Bus objects and export them on a
    connection, allowing other programs to acccess them. *)

(** {6 Types} *)

type 'a t
  (** Type of local D-Bus objects. It contains informations needed by
      obus to export it on a connection and dispatch incoming method
      calls.

      ['a] is the type of value that may be attached to this
      object. *)

type 'a interface
  (** An interface description *)

type 'a method_info
  (** Informations about a method *)

type 'a signal_info
  (** Informations about a signal *)

type 'a property_info
  (** Informations about a property *)

(** {6 Objects creation} *)

val attach : 'a t -> 'a -> unit
  (** [attach obus_object custom_obejct] attaches [custom_object] to
      [obus_object]. [custom_object] will be the value received by
      method call handlers. Note that you need to attach the object
      before you can export it on a coneection and you can not attach
      an object multiple times. *)

val get : 'a t -> 'a
  (** [get obj] returns the data attached to the given object *)

val make : ?owner : OBus_peer.t -> ?common : bool -> ?interfaces : 'a interface list -> OBus_path.t -> 'a t
  (** [make ?owner ?common ?interfaces path] creates a new D-Bus
      object with path [path].

      If [owner] is specified, then:
      - all signals will be sent to it by default,
      - the object will be removed from all its exports when the owner exits,
      - it will automatically be exported on the connection of the owner when
        [attach] is invoked.

      [interfaces] is the list of interfaces implemented by the
      object. New interfaces can be added latter with
      {!add_interfaces}. If [common] is [true] (the default) then
      {!introspectable} and {!properties} are automatically added. *)

(** {6 Properties} *)

val path : 'a t -> OBus_path.t
  (** [path obj] returns the path of the object *)

val owner : 'a t -> OBus_peer.t option
  (** [owner obj] returns the owner of the object, if any *)

val exports : 'a t -> Set.Make(OBus_connection).t React.signal
  (** [exports obj] is a signal holding the list of connnections on
      which the object is exported. *)

val introspect : 'a t -> OBus_introspect.interface list
  (** [introspect obj] returns the introspection of all interfaces
      implemented by [obj] *)

val on_properties_changed : 'a t -> (OBus_name.interface -> (OBus_name.member * OBus_value.V.single option) list -> unit Lwt.t) ref
  (** Function called when one or more properties of the given object
      change. The new contents of the property is given along with the
      property name according to the
      [org.freedesktop.DBus.Property.EmitsChangedSignal].

      The default function uses the standard
      [org.freedesktop.DBus.Properties.PropertiesChanged] signal. *)

(** {6 Exports} *)

val export : OBus_connection.t -> 'a t -> unit
  (** [export connection obj] exports [obj] on [connection]. It raises
      {!OBus_connection.Connection_closed} if the connection is closed. *)

val remove : OBus_connection.t -> 'a t -> unit
  (** [remove connection obj] removes [obj] from [connection]. It does
      nothing if the connection is closed. *)

val remove_by_path : OBus_connection.t -> OBus_path.t -> unit
  (** [remove_by_path connection path] removes the object with path
      [path] on [connection]. It works for normal objects and dynamic
      nodes. It does nothing if the connection is closed. *)

val destroy : 'a t -> unit
  (** [destroy obj] removes [obj] from all connection it is exported
      on *)

val dynamic :
  connection : OBus_connection.t ->
  prefix : OBus_path.t ->
  handler : (OBus_context.t -> OBus_path.t -> 'a t Lwt.t) -> unit
  (** [dynamic ~connection ~prefix ~handler] defines a dynamic node in
      the tree of object. This means that objects with a path prefixed
      by [prefix], will be created on the fly by [handler] when a
      process try to access them.

      [handler] receive the context and rest of path after the
      prefix. It may raises [Not_found] to indicates that there is no
      object under the given path.

      Note: if you manually export an object with a path prefixed by
      [prefix], it will have precedence over the one created by
      [handler]. *)

(** {6 Interfaces} *)

val make_interface : name : OBus_name.interface ->
  ?annotations : OBus_introspect.annotation list ->
  ?methods : 'a method_info list ->
  ?signals : 'a signal_info list ->
  ?properties : 'a property_info list -> unit -> 'a interface
  (** [make_interface ~name ?annotations ?methods ?signals ?properties ()]
      creates a new interface *)

(**/**)

val make_interface_unsafe : OBus_name.interface ->
  OBus_introspect.annotation list ->
  'a method_info array ->
  'a signal_info array ->
  'a property_info array -> 'a interface

(**/**)

val add_interfaces : 'a t -> 'a interface list -> unit
  (** [add_interfaces obj ifaces] adds suport for the interfaces
      described by [ifaces] to the given object. If an interface with
      the same name is already attached to the object, then it is
      replaced by the new one. *)

val remove_interfaces : 'a t -> 'a interface list -> unit
  (** [remove_interaces obj ifaces] removes informations about the
      given interfaces from [obj]. If [obj] does not implement some of
      the interfaces, it does nothing. *)

val remove_interfaces_by_names : 'a t -> OBus_name.interface list -> unit
  (** Same as {!remove_interfaces} but takes only the interface names
      as argument. *)

(** {8 Well-known interfaces} *)

val introspectable : unit -> 'a interface
  (** The [org.freedesktop.DBus.Introspectable] interface *)

val properties : unit -> 'a interface
  (** The [org.freedesktop.DBus.Properties] interface *)

(** {6 Members} *)

val method_info : ('a, 'b) OBus_member.Method.t -> ('c t -> 'a -> 'b Lwt.t) -> 'c method_info
  (** [method_info desc handler] creates a method-call
      member. [handler] receive the destination object of the method
      call and the arguments of the method call. The context of the
      call is also available to [handler] by using
      {!OBus_context.get}. *)

val signal_info : 'a OBus_member.Signal.t -> 'b signal_info
  (** Defines a signal. It is only used for introspection *)

val property_r_info : ('a, [ `readable ]) OBus_member.Property.t -> ('b t -> 'a React.signal) -> 'b property_info
  (** [property_r_info desc get] defines a read-only property. [get]
      is called once when data is attached to an object with
      {!attach}. It must return a signal holding the current value of
      the property. *)

val property_w_info : ('a, [ `writable ]) OBus_member.Property.t -> ('b t -> 'a -> unit Lwt.t) -> 'b property_info
  (** [property_w_info desc set] defines a write-only property. [set]
      is used to set the propertry contents. *)

val property_rw_info : ('a, [ `readable | `writable ]) OBus_member.Property.t -> ('b t -> 'a React.signal) -> ('b t -> 'a -> unit Lwt.t) -> 'b property_info
  (** [property_rw_info desc get set] defines a readable and writable
      property. [get] and [set] have the same semantic as for
      {!property_r_info} and {!property_w_info}. *)

(** {6 Signals} *)

val emit : 'a t ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  ?peer : OBus_peer.t ->
  'b OBus_value.C.sequence -> 'b -> unit Lwt.t
  (** [emit obj ~interface ~member ?peer typ args] emits a signal. it
      uses the same rules as {!OBus_signal.emit} for choosing the
      destinations of the signal.  *)
