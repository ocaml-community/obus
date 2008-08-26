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

type t = OBus_internals.proxy

val compare : t -> t -> int
  (** Proxy comparaison function *)

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

val method_call : ?interface:OBus_name.Interface.t -> member:OBus_name.Member.t -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> t -> 'a
  (** Send a method call on a proxy *)

val kmethod_call : ((t -> 'b Lwt.t) -> 'c) -> ?interface:OBus_name.Interface.t -> member:OBus_name.Member.t -> ('a, 'c, 'b) OBus_type.ty_function -> 'a
  (** Same thing but with continuation *)

val dmethod_call : ?interface:OBus_name.Interface.t -> member:OBus_name.Member.t -> t -> OBus_message.body -> OBus_message.body Lwt.t
  (** Send a method call with dynamically typed datas *)

val on_signal : interface:OBus_name.Interface.t -> member:OBus_name.Member.t ->
  ('a, unit, unit) OBus_type.ty_function -> t -> 'a -> OBus_signal.receiver Lwt.t
  (** Connect a callback function to the given signal. It is possible
      to connect multiple functions to the same signal. *)

val don_signal : interface:OBus_name.Interface.t -> member:OBus_name.Member.t ->
  t -> (OBus_message.body -> unit) -> OBus_signal.receiver Lwt.t
  (** Dynamically-typed version *)

val property : interface:OBus_name.Interface.t -> name:OBus_name.Member.t -> access:([< OBus_property.access ] as 'b) -> [< 'a OBus_type.cl_single ] -> t -> ('a, 'b) OBus_property.t
  (** Create a property *)

val dproperty : interface:OBus_name.Interface.t -> name:OBus_name.Member.t -> access:([< OBus_property.access ] as 'a) -> t -> 'a OBus_property.dt
  (** Dynamically-typed version *)
