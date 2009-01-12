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
  val call : string -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> t -> 'a
  val signal : ?broadcast:bool -> OBus_name.member -> [< 'a OBus_type.cl_sequence ] -> t -> 'a OBus_signal.t
  val property : string -> ([< OBus_property.access ] as 'b) -> [< 'a OBus_type.cl_single ] -> t -> ('a, 'b) OBus_property.t
  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
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
  let call member typ proxy = OBus_proxy.call ~interface ~member proxy typ
  let signal ?broadcast member typ proxy = OBus_signal.make ?broadcast ~interface ~member typ (fun _ -> return proxy)
  let property member access typ proxy = OBus_property.make ~interface ~member ~access typ (fun _ -> return proxy)
  let register_exn error_name = OBus_error.register (interface ^ "." ^ error_name)
end

module Make_custom(Proxy : Custom_proxy)(Name : Name) =
struct
  type t = Proxy.t
  let interface = Name.name
  let call member typ obj =
    OBus_type.make_func typ
      (fun body -> perform
         proxy <-- Proxy.make_proxy obj;
         OBus_proxy.call' proxy ~interface ~member body (OBus_type.func_reply typ))
  let signal ?broadcast member typ obj = OBus_signal.make ?broadcast ~interface ~member typ (fun _ -> Proxy.make_proxy obj)
  let property member access typ obj = OBus_property.make ~interface ~member ~access typ (fun _ -> Proxy.make_proxy obj)
  let register_exn error_name = OBus_error.register (interface ^ "." ^ error_name)
end

module Make_single(Proxy : Single_proxy)(Name : Name) =
struct
  let interface = Name.name
  let call member typ =
    OBus_type.make_func typ
      (fun body -> perform
         proxy <-- Lazy.force Proxy.proxy;
         OBus_proxy.call' proxy ~interface ~member body (OBus_type.func_reply typ))
  let signal ?broadcast member typ = OBus_signal.make ?broadcast ~interface ~member typ (fun _ -> Lazy.force Proxy.proxy)
  let property member access typ = OBus_property.make ~interface ~member ~access typ (fun _ -> Lazy.force Proxy.proxy)
  let register_exn error_name = OBus_error.register (interface ^ "." ^ error_name)
end
