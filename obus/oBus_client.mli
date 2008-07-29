(*
 * oBus_client.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Library binding construction *)

(** This module let you create a service binding. To do that you have
    to create a module for the service, then use it to create a module
    for each interface and use them to define method calls.

    The [obus-binder] tool can create binding skeleton from XML
    introspection file.  *)

(** Interface parameters *)
module type Interface_params = sig
  type t
    (** Type of objects. Without customisation it is an object of type
        [OBus_proxy.t] *)

  val name : string
    (** Interface name *)

  val of_proxy : OBus_proxy.t -> t
    (** Tell how to create an object of type [t] from a proxy *)

  val to_proxy : t -> OBus_proxy.t
    (** Tell how to create a proxy from an object of type [t] *)
end

(** A DBus interface *)
module type Interface = sig
  type t
    (** Type of objects *)

  val of_proxy : OBus_proxy.t -> t
  val to_proxy : t -> OBus_proxy.t
    (** Herited from interface parameters *)

  val ob_t : (t, _, OBus_annot.dobject_path) OBus_comb.one
    (** Combinator, so you can use this type in method or signal
        type. *)

  val call : t -> string -> ('a, 'b Lwt.t, 'b) OBus_comb.func -> 'a
    (** [call obj member typ] call a method. *)

  val kcall : ('b -> 'c Lwt.t) -> t -> string -> ('a, 'c Lwt.t, 'b) OBus_comb.func -> 'a
    (** Same thing but with continuation *)

  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
    (** Same as [OBus_error.register] but the error name will be
        prefixed by the interface name *)
end

(** Service parameters *)
module type Service_params = sig
  val name : string
end

(** DBus service *)
module type Service = sig
  (** Create and interface *)
  module Make(Params : Interface_params) : Interface
    with type t = Params.t

  (** Create an interface but without customized type *)
  module Make_simple(Params : sig val name : string end) : Interface
    with type t = OBus_proxy.t
end

(** Create a service *)
module Make_service(Params : Service_params) : Service
