(*
 * oBus_client.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Library binding construction *)

(** This module define helpers for creating proxy code. It handle some
    of the most common cases, but if it does not fit your needs you
    can directly use [OBus_proxy] or [OBus_connection].

    The [obus-binder] tool can create a skeleton from XML
    introspection file. *)

(** {6 DBus interface} *)

module type Interface = sig
  type t
    (** Type of objects, without customization it is [OBus_proxy.t] *)

  val call : OBus_name.Member.t -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> t -> 'a
    (** [call obj member typ ...] call a method. *)

  val kcall : ((t -> 'b Lwt.t) -> 'c) -> OBus_name.Member.t -> ('a, 'c, 'b) OBus_type.ty_function -> 'a
    (** Same thing but with continuation *)

  val dcall : OBus_name.Member.t -> t -> OBus_message.body -> OBus_message.body Lwt.t
    (** Dynamically typed version. *)

  val on_signal : ?global:bool -> OBus_name.Member.t -> ('a, unit, unit) OBus_type.ty_function -> t -> 'a -> OBus_signal.receiver Lwt.t
    (** [on_signal member typ obj func] register a
        callback function for the given signal *)

  val don_signal : ?global:bool -> OBus_name.Member.t -> t -> (OBus_message.body -> unit) -> OBus_signal.receiver Lwt.t
    (** Dynamically typed version. *)

  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
    (** Same as [OBus_error.register] but the error name will be
        prefixed by the interface name *)

  val property : OBus_name.Member.t -> ([< OBus_property.access ] as 'b) -> [< 'a OBus_type.cl_single ] -> t -> ('a, 'b) OBus_property.t
  val dproperty : OBus_name.Member.t -> ([< OBus_property.access ] as 'a) -> t -> 'a OBus_property.dt
    (** Creation of properties *)
end

(** {6 Common case} *)

module Make(Name : sig val name : OBus_name.Interface.t end) : Interface
  with type t = OBus_proxy.t

(** {6 Interface with customized object type} *)

module type Custom_params = sig
  type t
    (** Type of objects *)

  val name : OBus_name.Interface.t
    (** Interface name *)

  val to_proxy : t -> OBus_proxy.t
    (** Tell how to create a proxy from an object of type [t] *)
end

module Make_custom(Params : Custom_params) : Interface
  with type t = Params.t

(** {6 Interface for single object} *)

(** Interface implemented by only one object with a constant path *)

module type Constant_path_params = sig
  val name : OBus_name.Interface.t
  val path : OBus_path.t
  val service : OBus_name.Bus.t option
end

module Make_constant_path(Params : Constant_path_params) : Interface
  with type t = OBus_connection.t

(** {6 Constant message bus and service} *)

module type Constant_bus_params = sig
  val name : OBus_name.Interface.t
  val service : OBus_name.Bus.t option
  val bus : OBus_connection.t Lwt.t Lazy.t
end

module Make_constant_bus(Params : Constant_bus_params) : Interface
  with type t = OBus_path.t

(** {6 Everything constant} *)

module type Constant_params = sig
  val name : OBus_name.Interface.t
  val path : OBus_path.t
  val service : OBus_name.Bus.t option
  val bus : OBus_connection.t Lwt.t Lazy.t
end

module Make_constant(Params : Constant_params) : sig
  val call : OBus_name.Member.t -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> 'a
  val kcall : ('b Lwt.t -> 'c) -> OBus_name.Member.t -> ('a, 'c, 'b) OBus_type.ty_function -> 'a
  val dcall : OBus_name.Member.t -> OBus_message.body -> OBus_message.body Lwt.t
  val on_signal : ?global:bool -> OBus_name.Member.t -> ('a, unit, unit) OBus_type.ty_function -> 'a -> OBus_signal.receiver Lwt.t
  val don_signal : ?global:bool -> OBus_name.Member.t -> (OBus_message.body -> unit) -> OBus_signal.receiver Lwt.t
  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
  val property : OBus_name.Member.t -> ([< OBus_property.access ] as 'b) -> [< 'a OBus_type.cl_single ] -> ('a, 'b) OBus_property.t
  val dproperty : OBus_name.Member.t -> ([< OBus_property.access ] as 'a) -> 'a OBus_property.dt
end
