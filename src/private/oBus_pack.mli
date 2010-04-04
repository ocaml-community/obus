(*
 * oBus_pack.mli
 * -------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Packed values *)

type t
  (** Type of a pack. A pack is a box containing one value of any
      type. *)

val dummy : t
  (** A dummy pack that does not contain anything *)

(** Signature for pack contents *)
module type Element = sig
  type t
    (** Type of values we want to put into a pack *)
end

(** Signature of packer/unpacker *)
module type S = sig
  type element
    (** Type of values we want to pack/unpack *)

  val pack : element -> t
    (** Pack a value *)

  val unpack : t -> element
    (** Unpack a value. It raises [Invalid_argument] if the pack was
        not created with the corresponding {!pack} *)
end

(** Functors which create a pair of packer/unpacker for the given
    elemene type *)
module Make(Element : Element) : S with type element = Element.t
