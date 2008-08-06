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

  val call : t -> string -> ('a, 'b Lwt.t, 'b, _, _) OBus_comb.func -> 'a
    (** [call obj member typ] call a method. *)

  val kcall : ('b Lwt.t -> 'c) -> t -> string -> ('a, 'c, 'b, _, _) OBus_comb.func -> 'a
    (** Same thing but with continuation *)

  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
    (** Same as [OBus_error.register] but the error name will be
        prefixed by the interface name *)
end

(** {6 Common case} *)

module Make(Name : sig val name : string end) : Interface
  with type t = OBus_proxy.t

(** {6 Interface with customized object type} *)

module type Custom_params = sig
  type t
    (** Type of objects *)

  val name : string
    (** Interface name *)

  val of_proxy : OBus_proxy.t -> t
    (** Tell how to create an object of type [t] from a proxy *)

  val to_proxy : t -> OBus_proxy.t
    (** Tell how to create a proxy from an object of type [t] *)
end

module Make_custom(Params : Custom_params) : Interface
  with type t = Params.t

(** {6 Interface for single object} *)

(** Interface implemented by only one object with a fixed path *)

module type Fixed_path_params = sig
  val name : string
  val path : OBus_path.t
  val service : string option
end

module Make_fixed_path(Params : Fixed_path_params) : Interface
  with type t = OBus_connection.t

(** {6 Fixed message bus and service} *)

module type Fixed_bus_params = sig
  val name : string
  val service : string option
  val bus : OBus_connection.t Lwt.t Lazy.t
end

module Make_fixed_bus(Params : Fixed_bus_params) : Interface
  with type t = OBus_path.t

(** {6 Everything fixed} *)

module type Fixed_params = sig
  val name : string
  val path : OBus_path.t
  val service : string option
  val bus : OBus_connection.t Lwt.t Lazy.t
end

module Make_fixed(Params : Fixed_params) : sig
  val call : string -> ('a, 'b Lwt.t, 'b, _, _) OBus_comb.func -> 'a
  val kcall : ('b Lwt.t -> 'c) -> string -> ('a, 'c, 'b, _, _) OBus_comb.func -> 'a
  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
end
