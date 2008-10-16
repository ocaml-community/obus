(*
 * oBus_client.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type Custom_params = sig
  type t
  val name : string
  val to_proxy : t -> OBus_proxy.t
end
module type Interface = sig
  type t
  val call : string -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> t -> 'a
  val kcall : ((t -> 'b Lwt.t) -> 'c) -> string -> ('a, 'c, 'b) OBus_type.ty_function -> 'a
  val dcall : string -> t -> OBus_value.sequence -> OBus_value.sequence Lwt.t
  val on_signal : ?global:bool -> OBus_name.member -> [< 'a OBus_type.cl_sequence ] -> t -> ('a -> unit) -> OBus_signal.receiver Lwt.t
  val don_signal : ?global:bool -> string -> t -> (OBus_value.sequence -> unit) -> OBus_signal.receiver Lwt.t
  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
  val property : string -> ([< OBus_property.access ] as 'b) -> [< 'a OBus_type.cl_single ] -> t -> ('a, 'b) OBus_property.t
  val dproperty : string -> ([< OBus_property.access ] as 'a) -> t -> 'a OBus_property.dt
end
module type Constant_path_params = sig
  val name : string
  val path : OBus_path.t
  val service : string option
end
module type Constant_bus_params = sig
  val name : string
  val service : string option
  val bus : OBus_connection.t Lwt.t Lazy.t
end
module type Constant_params = sig
  val name : string
  val path : OBus_path.t
  val service : string option
  val bus : OBus_connection.t Lwt.t Lazy.t
end

let register_exn interface error_name = OBus_error.register (interface ^ "." ^ error_name)

module Make(Name : sig val name : string end) =
struct
  type t = OBus_proxy.t
  let call member = OBus_proxy.method_call ~interface:Name.name ~member
  let kcall cont member = OBus_proxy.kmethod_call cont ~interface:Name.name ~member
  let dcall member = OBus_proxy.dmethod_call ~interface:Name.name ~member
  let on_signal ?global member = OBus_proxy.on_signal ?global ~interface:Name.name ~member
  let don_signal ?global member = OBus_proxy.don_signal ?global ~interface:Name.name ~member
  let property name access = OBus_proxy.property ~interface:Name.name ~name ~access
  let dproperty name access = OBus_proxy.dproperty ~interface:Name.name ~name ~access
  let register_exn = register_exn Name.name
end

module Make_custom(Params : Custom_params) =
struct
  include Params

  let call member typ obj =
    OBus_proxy.method_call ~interface:name ~member typ (to_proxy obj)
  let kcall cont member =
    OBus_proxy.kmethod_call (fun f -> cont (fun obj -> f (to_proxy obj))) ~interface:name ~member
  let dcall member obj body =
    OBus_proxy.dmethod_call ~interface:name ~member (to_proxy obj) body
  let on_signal ?global member typ obj f = OBus_proxy.on_signal ?global ~interface:Params.name ~member typ (to_proxy obj) f
  let don_signal ?global member obj f = OBus_proxy.don_signal ?global ~interface:Params.name ~member (to_proxy obj) f
  let property name access typ obj = OBus_proxy.property ~interface:Params.name ~name ~access typ (to_proxy obj)
  let dproperty name access obj = OBus_proxy.dproperty ~interface:Params.name ~name ~access (to_proxy obj)

  let register_exn = register_exn Params.name
end

open OBus_connection

module Make_constant_path(Params : Constant_path_params) =
struct
  type t = OBus_connection.t

  let call member typ connection =
    method_call connection
      ?destination:Params.service
      ~path:Params.path
      ~interface:Params.name
      ~member
      typ

  let kcall cont member typ =
    kmethod_call (fun f -> cont f)
      ?destination:Params.service
      ~path:Params.path
      ~interface:Params.name
      ~member
      typ

  let dcall member connection body =
    dmethod_call connection
      ?destination:Params.service
      ~path:Params.path
      ~interface:Params.name
      ~member body

  let on_signal ?global member typ connection f =
    OBus_signal.add_receiver connection ?global
      ?sender:Params.service
      ~path:Params.path
      ~interface:Params.name
      ~member typ f

  let don_signal ?global member connection f =
    OBus_signal.dadd_receiver connection ?global
      ?sender:Params.service
      ~path:Params.path
      ~interface:Params.name
      ~member f

  let property name access typ connection =
    OBus_property.make
      ~connection
      ?destination:Params.service
      ~interface:Params.name
      ~path:Params.path
      ~name
      ~access
      typ

  let dproperty name access connection =
    OBus_property.dmake
      ~connection
      ?destination:Params.service
      ~interface:Params.name
      ~path:Params.path
      ~access
      ~name

  let register_exn = register_exn Params.name
end

module Make_constant_bus(Params : Constant_bus_params) =
struct
  type t = OBus_path.t


  open OBus_internals
  open Lwt

  let kcall cont member ty =
    call_and_cast_reply ty & fun body f ->
      cont & fun path ->
        Lazy.force Params.bus >>= fun bus ->
          f bus (OBus_message.method_call
                   ?destination:Params.service
                   ~path
                   ~interface:Params.name
                   ~member body)

  let call member typ path = kcall (fun f -> f path) member typ

  let dcall member path body =
    Lazy.force Params.bus >>= fun bus ->
      dmethod_call bus
        ?destination:Params.service
        ~path
        ~interface:Params.name
        ~member body

  let on_signal ?global member typ path f =
    Lazy.force Params.bus >>= fun bus ->
      OBus_signal.add_receiver bus ?global
        ?sender:Params.service ~path ~interface:Params.name ~member typ f

  let don_signal ?global member path f =
    Lazy.force Params.bus >>= fun bus ->
      OBus_signal.dadd_receiver bus ?global
        ?sender:Params.service ~path ~interface:Params.name ~member f

  let property name access typ path =
    OBus_property.lmake
      ~connection:Params.bus
      ?destination:Params.service
      ~interface:Params.name
      ~path
      ~name
      ~access
      typ

  let dproperty name access path =
    OBus_property.ldmake
      ~connection:Params.bus
      ?destination:Params.service
      ~interface:Params.name
      ~path
      ~name
      ~access

  let register_exn = register_exn Params.name
end

module Make_constant(Params : Constant_params) = struct
  include Make_constant_bus(Params)
  let kcall cont = kcall (fun f -> cont (f Params.path))
  let call member typ = call member typ Params.path
  let dcall member = dcall member Params.path
  let on_signal ?global member typ f = on_signal ?global member typ Params.path f
  let don_signal ?global member = don_signal ?global member Params.path
  let property name access typ = property name access typ Params.path
  let dproperty name access = dproperty name access Params.path
end
