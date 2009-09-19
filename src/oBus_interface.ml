(*
 * oBus_interface.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

module type S = sig
  type t
  val interface : OBus_name.interface
  val method_call : string -> ('a, 'b Lwt.t, 'b) OBus_type.func -> t -> 'a
  val signal : ?broadcast:bool -> OBus_name.member -> ('a, _) OBus_type.cl_sequence -> t -> 'a OBus_signal.t
  val property : string -> 'mode OBus_property.access -> ('a, _) OBus_type.cl_single -> t -> ('a, 'mode) OBus_property.t
  val property_r : OBus_name.member -> ('a, _) OBus_type.cl_single -> t -> ('a, [ `readable ]) OBus_property.t
  val property_w : OBus_name.member -> ('a, _) OBus_type.cl_single -> t -> ('a, [ `writable ]) OBus_property.t
  val property_rw : OBus_name.member -> ('a, _) OBus_type.cl_single -> t -> ('a, [ `readable | `writable ]) OBus_property.t
end

module type Name = sig
  val name : OBus_name.interface
end

module type Custom_proxy = sig
  type t
  val make_proxy : t -> OBus_proxy.t Lwt.t
end

module type Single_proxy = sig
  val proxy : OBus_proxy.t Lwt.t Lazy.t
end

module Make(Name : Name) =
struct
  type t = OBus_proxy.t
  let interface = Name.name
  let method_call member typ proxy = OBus_proxy.method_call ~interface ~member proxy typ
  let signal ?broadcast member typ proxy = OBus_signal.make ?broadcast ~interface ~member typ proxy
  let property member access typ proxy = OBus_property.make ~interface ~member ~access typ proxy
  let property_r member typ proxy = property member OBus_property.rd_only typ proxy
  let property_w member typ proxy = property member OBus_property.wr_only typ proxy
  let property_rw member typ proxy = property member OBus_property.rdwr typ proxy
end

module Make_custom(Proxy : Custom_proxy)(Name : Name) =
struct
  type t = Proxy.t
  let interface = Name.name
  let method_call member typ obj =
    OBus_type.make_func typ
      (fun body -> perform
         proxy <-- Proxy.make_proxy obj;
         OBus_proxy.method_call' proxy ~interface ~member body (OBus_type.func_reply typ))
  let signal ?broadcast member typ obj = OBus_signal.make_custom ?broadcast ~interface ~member typ (fun _ -> Proxy.make_proxy obj)
  let property member access typ obj = OBus_property.make_custom ~interface ~member ~access typ (fun _ -> Proxy.make_proxy obj)
  let property_r member typ obj = property member OBus_property.rd_only typ obj
  let property_w member typ obj = property member OBus_property.wr_only typ obj
  let property_rw member typ obj = property member OBus_property.rdwr typ obj
end

module Make_single(Proxy : Single_proxy)(Name : Name) =
struct
  let interface = Name.name
  let method_call member typ =
    OBus_type.make_func typ
      (fun body -> perform
         proxy <-- Lazy.force Proxy.proxy;
         OBus_proxy.method_call' proxy ~interface ~member body (OBus_type.func_reply typ))
  let signal ?broadcast member typ = OBus_signal.make_custom ?broadcast ~interface ~member typ (fun _ -> Lazy.force Proxy.proxy)
  let property member access typ = OBus_property.make_custom ~interface ~member ~access typ (fun _ -> Lazy.force Proxy.proxy)
  let property_r member typ = property member OBus_property.rd_only typ
  let property_w member typ = property member OBus_property.wr_only typ
  let property_rw member typ = property member OBus_property.rdwr typ
end
