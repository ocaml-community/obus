(*
 * oBus_interface.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type S = sig
  type t = OBus_proxy.t
  val tt : t OBus_type.ty_basic
  val interface : OBus_name.interface
  val call : string -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> t -> 'a
  val signal : ?broadcast:bool -> OBus_name.member -> [< 'a OBus_type.cl_sequence ] -> 'a OBus_signal.t
  val property : string -> ([< OBus_property.access ] as 'b) -> [< 'a OBus_type.cl_single ] -> ('a, 'b) OBus_property.t
  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
end

module Make(Name : sig val name : string end) =
struct
  type t = OBus_proxy.t
  let tt = OBus_proxy.tt
  let interface = Name.name
  let call member typ proxy = OBus_proxy.call ~interface ~member proxy typ
  let signal ?broadcast member typ = OBus_signal.make ?broadcast ~interface ~member typ
  let property member access typ = OBus_property.make ~interface ~member ~access typ
  let register_exn error_name = OBus_error.register (interface ^ "." ^ error_name)
end
