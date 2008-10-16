(*
 * oBus_proxy.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Representation of DBus proxies *)

(** A proxy is an object on which live on a different processus, but
    behave as a native ocaml value. *)

type t

val tt : t OBus_type.ty_basic
  (** Type combinator *)

(** {6 Creation/informations} *)

val make : connection:OBus_connection.t -> ?destination:OBus_name.Connection.t ->  path:OBus_path.t -> t
  (** [make connection service path] create a proxy,

      - [connection] is used for serializing method calls on the object
      - [destination] is the connection name of the application owning
        the object
      - [path] is the path of the object on the application owning it *)

val connection : t -> OBus_connection.t
val destination : t -> OBus_name.Connection.t option
val path : t -> OBus_path.t
  (** Access to proxy informations *)

(** {6 Method calls} *)

val method_call : t -> ?interface:OBus_name.interface -> member:OBus_name.member -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> 'a
  (** Send a method call on a proxy *)

val kmethod_call : ((t -> 'b Lwt.t) -> 'c) -> ?interface:OBus_name.interface -> member:OBus_name.member -> ('a, 'c, 'b) OBus_type.ty_function -> 'a
  (** Same thing but with continuation *)

val dmethod_call : t -> ?interface:OBus_name.interface -> member:OBus_name.member -> OBus_message.body -> OBus_message.body Lwt.t
  (** Send a method call with dynamically typed datas *)

(** {6 Signals} *)

val on_signal : t -> ?global:bool -> interface:OBus_name.interface -> member:OBus_name.member ->
  [< 'a OBus_type.cl_sequence ] -> ('a -> unit) -> OBus_signal.receiver Lwt.t
  (** Connect a callback function to the given signal. It is possible
      to connect multiple functions to the same signal. *)

val don_signal : t -> ?global:bool -> interface:OBus_name.interface -> member:OBus_name.member ->
  (OBus_message.body -> unit) -> OBus_signal.receiver Lwt.t
  (** Dynamically-typed version *)

(** {6 Creation of properties} *)

val property : t -> interface:OBus_name.interface -> member:OBus_name.member -> access:([< OBus_property.access ] as 'b) -> [< 'a OBus_type.cl_single ] -> ('a, 'b) OBus_property.t
  (** Create a property *)

val dproperty : t -> interface:OBus_name.interface -> member:OBus_name.member -> access:([< OBus_property.access ] as 'a) -> 'a OBus_property.dt
  (** Dynamically-typed version *)

(** {6 Direct access to properties} *)

val set : t -> interface:OBus_name.interface -> member:OBus_name.member -> [< 'a OBus_type.cl_single ] -> 'a -> unit Lwt.t
  (** Set the value of a property *)

val get : t -> interface:OBus_name.interface -> member:OBus_name.member -> [< 'a OBus_type.cl_single ] -> 'a Lwt.t
  (** Get the value of a property *)

val dset : t -> interface:OBus_name.interface -> member:OBus_name.member -> OBus_value.single -> unit Lwt.t
val dget : t -> interface:OBus_name.interface -> member:OBus_name.member -> OBus_value.single Lwt.t
  (** Dynamically-typed version *)

val dget_all : t -> interface:OBus_name.interface -> (OBus_name.member * OBus_value.single) list Lwt.t
  (** Retreive all properties of an interface *)
