(*
 * oBus_object.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Local D-Bus objects *)

(** This module allow you to create D-Bus objects and export them on a
    connection, allowing other programs to acccess them. *)

(** {6 Types} *)

type 'a t
  (** Type of local D-Bus objects. It contains informations needed by
      obus to export it on a connection and dispatch incomming method
      calls.

      ['a] is the type of value that may be attached to this
      object. *)

type 'a interface
  (** An interface description *)

type 'a member
  (** Part of an inteface *)

type 'a notify_mode
  (** Describe how property changes should be announced to other
      applications *)

(** {6 Objects creation} *)

val attach : 'a t -> 'a -> unit
  (** [attach obus_object custom_obejct] attaches [custom_object] to
      [obus_object]. [custom_object] will be the value received by
      method call handlers. Note that you need to attach the object
      before you can export it on a coneection.

      Note that you can not attach an object multiple times. *)

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
      {!add_interface}. If [common] is [true] (the default) then
      {!introspectable} and {!properties} are automatically added. *)

(** {6 Properties} *)

val path : 'a t -> OBus_path.t
  (** [path obj] returns the path of the object *)

val owner : 'a t -> OBus_peer.t option
  (** [owner obj] returns the owner of the object, if any *)

val exports : 'a t -> Set.Make(OBus_connection).t
  (** [exports obj] returns the set of connnections on which the
      object is exproted *)

val introspect : 'a t -> OBus_introspect.interface list
  (** [introspect obj] returns the introspection of all interfaces
      implemented by [obj] *)

(** {6 Exports} *)

val export : OBus_connection.t -> 'a t -> unit
  (** [export connection obj] exports [obj] on [connection] *)

val remove : OBus_connection.t -> 'a t -> unit
  (** [remove connection obj] removes [obj] from [connection] *)

val remove_by_path : OBus_connection.t -> OBus_path.t -> unit
  (** [remove_by_path connection path] removes the object with path
      [path] on [connection]. Works for normal objects and dynamic
      nodes. *)

val destroy : 'a t -> unit
  (** [destroy obj] removes [obj] from all connection it is exported
      on *)

val dynamic : connection : OBus_connection.t -> prefix : OBus_path.t -> handler : (OBus_path.t -> 'a t Lwt.t) -> unit
  (** [dynamic ~connection ~prefix ~handler] defines a dynamic node in
      the tree of object. This means that objects with a path prefixed
      by [prefix], will be created on the fly by [handler] when a
      process try to access them.

      [handler] receive the rest of path after the prefix.

      Note: if you manually export an object with a path prefixed by
      [prefix], it will have precedence over the one created by
      [handler]. *)

(** {6 Interfaces} *)

val make_interface : ?notify_mode : 'a notify_mode -> OBus_name.interface -> 'a member list -> 'a interface
  (** [make_interface ?notify_mode ?sorted name members] creates a new
      interface. [notify_mode] determines how property changes are
      announced. It defaults to {!notify_none}. *)

(**/**)

val make_interface_unsafe : ?notify_mode : 'a notify_mode -> OBus_name.interface ->
  'a member array ->
  'a member array ->
  'a member array -> 'a interface

(**/**)

val add_interface : 'a t -> 'a interface -> unit
  (** [add_interface obj iface] adds suport for the interface
      described by [iface] to the given object. If an interface with
      the same name is already attached to the object, then it is
      replaced by the new one. *)

val remove_interface : 'a t -> 'a interface -> unit
  (** [remove_interace obj iface] removes informations about the given
      interface from [obj]. If [obj] do not implement the interface,
      it does nothing. *)

val remove_interface_by_name : 'a t -> OBus_name.interface -> unit
  (** Same as {!remove_interface} by takes only the interface name as
      argument. *)

(** {8 Well-known interfaces} *)

val introspectable : unit -> 'a interface
  (** The [org.freedesktop.DBus.Introspectable] interface *)

val properties : unit -> 'a interface
  (** The [org.freedesktop.DBus.Properties] interface *)

(** {6 Members} *)

exception Done
  (** Indicates that a method call has been handled *)

val method_info : ('a, 'b) OBus_member.Method.t -> ('b OBus_context.t -> 'c -> 'a -> 'b Lwt.t) -> 'c member
  (** [method_info desc handler] creates a method-call
      member. [handler] may raise {!Done} to indicates that the reply
      have been sent. Otherwise obus will send the result of [f] as
      reply. *)

val signal_info : 'a OBus_member.Signal.t -> 'b member
  (** Defines a signal. It is only used for introspection *)

val property_r_info : ('a, [ `readable ]) OBus_member.Property.t -> ('b -> 'a React.signal) -> 'b member
  (** [property_r_info desc get] defines a read-only property. [get]
      is called once when data is attached to an object with
      {!attach}. It must returns a signal holding the current value of
      the property. *)

val property_w_info : ('a, [ `writable ]) OBus_member.Property.t -> (unit OBus_context.t -> 'b -> 'a -> unit Lwt.t) -> 'b member
  (** [property_w_info desc set] defines a write-only property. [set]
      is used to set the propertry contents. *)

val property_rw_info : ('a, [ `readable ]) OBus_member.Property.t -> ('b -> 'a React.signal) -> (unit OBus_context.t -> 'b -> 'a -> unit Lwt.t) -> 'b member
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

(** {6 Notification modes} *)

(** For a description of these modes, look at {!OBus_property} *)

val notify_none : 'a notify_mode
val notify_global : OBus_name.member -> 'a notify_mode
val notify_update : OBus_name.member -> 'a notify_mode

val notify_custom :
  emit : ('a t -> OBus_name.interface -> OBus_value.V.single Map.Make(String).t -> unit Lwt.t) ->
  signature : OBus_introspect.member list -> 'a notify_mode
  (** [notify_custom ~send ~signature] creates a custom notification
      mode. [emit] is responsible for sending change notifications.
      [emit] receives the mapping from property names to their values,
      for properties that have changed.

      [signature] is the signature to add to the interface when
      creating it. *)
