(*
 * oBus_client.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type Interface_params = sig
  type t
  val name : string
  val of_proxy : OBus_proxy.t -> t
  val to_proxy : t -> OBus_proxy.t
end
module type Interface = sig
  type t
  val of_proxy : OBus_proxy.t -> t
  val to_proxy : t -> OBus_proxy.t
  val ob_t : (t, _, OBus_annot.dobject_path) OBus_comb.one
  val call : t -> string -> ('a, 'b Lwt.t, 'b) OBus_comb.func -> 'a
  val kcall : ('b -> 'c Lwt.t) -> t -> string -> ('a, 'c Lwt.t, 'b) OBus_comb.func -> 'a
  val register_exn : OBus_error.name -> (OBus_error.message -> exn) -> (exn -> OBus_error.message option) -> unit
end
module type Service_params = sig
  val name : string
end
module type Service = sig
  module Make(Params : Interface_params) : Interface
    with type t = Params.t
  module Make_simple(Params : sig val name : string end) : Interface
    with type t = OBus_proxy.t
end

module Make_service(Params : Service_params) =
struct
  module Make(Params : Interface_params) =
  struct
    include Params

    let ob_t =
      let len = String.length name in
      let str = String.create (len + 2) in
        (* Create a module name from the interface name *)
        for i = 0 to len - 1 do
          if name.[i] = '.'
          then begin
            str.[i] <- '_';
            if i > 0 then str.[i - 1] <- Char.uppercase str.[i - 1]
          end else str.[i] <- name.[i];
          str.[len] <- '.';
          str.[len + 1] <- 't'
        done;
        OBus_comb.wrap ~annot:(OBus_annot.dproxy (Some str)) OBus_proxy.ob_t of_proxy to_proxy

    let call obj member =
      OBus_proxy.method_call (to_proxy obj) ~interface:name ~member
    let kcall cont obj member =
      OBus_proxy.kmethod_call cont (to_proxy obj) ~interface:name ~member

    let register_exn error_name = OBus_error.register (name ^ "." ^ error_name)
  end

  module Make_simple(Params : sig val name : string end) =
    Make(struct
           type t = OBus_proxy.t
           let name = Params.name
           let to_proxy x = x
           let of_proxy x = x
         end)
end
