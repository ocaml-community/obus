(*
 * oBus_pack.ml
 * ------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type t = exn

exception Dummy
let dummy = Dummy

module type Element = sig
  type t
end

module type S = sig
  type element
  val pack : element -> t
  val unpack : t -> element
end

module Make(Element : Element) =
struct
  type element = Element.t

  exception Pack of element

  let pack x = Pack x
  let unpack = function
    | Pack x -> x
    | _ -> invalid_arg "OBus_type.unpack"
end
