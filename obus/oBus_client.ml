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
  val call : t -> string -> ('a, 'b Lwt.t, 'b, _, _) OBus_comb.func -> 'a
  val kcall : ('b Lwt.t -> 'c) -> t -> string -> ('a, 'c, 'b, _, _) OBus_comb.func -> 'a
  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
end
module type Fixed_path_params = sig
  val name : string
  val path : string
  val service : string option
end
module type Fixed_bus_params = sig
  val name : string
  val service : string option
  val bus : OBus_connection.t Lwt.t Lazy.t
end
module type Fixed_params = sig
  val name : string
  val path : OBus_path.t
  val service : string option
  val bus : OBus_connection.t Lwt.t Lazy.t
end

let register_exn interface error_name = OBus_error.register (interface ^ "." ^ error_name)

module Make(Name : sig val name : string end) =
struct
  type t = OBus_proxy.t
  let call proxy member = OBus_proxy.method_call proxy ~interface:Name.name ~member
  let kcall cont proxy member = OBus_proxy.kmethod_call cont proxy ~interface:Name.name ~member
  let register_exn = register_exn Name.name
end

module Make_custom(Params : Custom_params) =
struct
  include Params

  let call obj member =
    OBus_proxy.method_call (to_proxy obj) ~interface:name ~member
  let kcall cont obj member =
    OBus_proxy.kmethod_call cont (to_proxy obj) ~interface:name ~member

  let register_exn = register_exn Params.name
end

module Make_fixed_path(Params : Fixed_path_params) =
struct
  type t = OBus_connection.t

  let kcall cont connection member =
    OBus_connection.ksend_message_with_reply (fun w -> cont (Lwt.bind w (fun (header, value) -> Lwt.return value)))
      connection
      (OBus_header.method_call
         ?destination:Params.service
         ~path:Params.path
         ~interface:Params.name
         ~member ())

  let call connection = kcall (fun x -> x) connection

  let register_exn = register_exn Params.name
end

module Make_fixed_bus(Params : Fixed_bus_params) =
struct
  type t = OBus_path.t

  open OBus_intern
  open Lwt

  let kcall cont path member typ =
    OBus_comb.func_make_writer typ $ fun writer ->
      let reply = OBus_comb.func_reply typ in
      cont
        (Lazy.force Params.bus >>= fun bus ->
           OBus_connection.wire_send_message_with_reply bus
             (OBus_header.method_call
                ?destination:Params.service
                ~path
                ~interface:Params.name
                ~member ())
             (OBus_comb.func_signature typ) writer
             (OBus_comb.annot reply) (OBus_comb.reader reply)
           >>= (snd |> return))

  let call path = kcall (fun x -> x) path

  let register_exn = register_exn Params.name
end

module Make_fixed(Params : Fixed_params) = struct
  include Make_fixed_bus(Params)
  let kcall cont = kcall cont Params.path
  let call member = call Params.path member
end
