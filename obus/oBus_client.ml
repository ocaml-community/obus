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
  val of_proxy : OBus_proxy.t -> t
  val to_proxy : t -> OBus_proxy.t
end
module type Interface = sig
  type t
  val call : string -> ('a, 'b Lwt.t, 'b) OBus_type.ty_function -> t -> 'a
  val kcall : ((t -> 'b Lwt.t) -> 'c) -> string -> ('a, 'c, 'b) OBus_type.ty_function -> 'a
  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
end
module type Constant_path_params = sig
  val name : string
  val path : string
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
  let register_exn = register_exn Name.name
end

module Make_custom(Params : Custom_params) =
struct
  include Params

  let call member typ obj =
    OBus_proxy.method_call ~interface:name ~member typ (to_proxy obj)
  let kcall cont member =
    OBus_proxy.kmethod_call (fun f -> cont (fun obj -> f (to_proxy obj))) ~interface:name ~member

  let register_exn = register_exn Params.name
end

let (&) a b = a b

module Make_constant_path(Params : Constant_path_params) =
struct
  type t = OBus_connection.t

  let kcall cont member =
    OBus_connection.ksend_message_with_reply & fun f ->
      cont
        (fun connection ->
           (Lwt.bind
              (f connection
                 (OBus_header.method_call
                    ?destination:Params.service
                    ~path:Params.path
                    ~interface:Params.name
                    ~member ()))
              (fun (header, value) -> Lwt.return value)))

  let call member typ connection = kcall (fun f -> f connection) member typ

  let register_exn = register_exn Params.name
end

module Make_constant_bus(Params : Constant_bus_params) =
struct
  type t = OBus_path.t

  open OBus_internals
  open Lwt

  let kcall cont member =
    OBus_connection.ksend_message_with_reply & fun f ->
      cont
        (fun path ->
           perform
             bus <-- Lazy.force Params.bus;
             (header, value) <-- f bus (OBus_header.method_call
                                          ?destination:Params.service
                                          ~path
                                          ~interface:Params.name
                                          ~member ());
             return value)

  let call member typ path = kcall (fun f -> f path) member typ

  let register_exn = register_exn Params.name
end

module Make_constant(Params : Constant_params) = struct
  include Make_constant_bus(Params)
  let kcall cont = kcall (fun f -> cont (f Params.path))
  let call member typ = call member typ Params.path
end
