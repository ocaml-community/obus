(*
 * common.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Shared code *)

module type TypesSig = sig
  type basic =
    | Byte
    | Boolean
    | Int16
    | Int32
    | Int64
    | Uint16
    | Uint32
    | Uint64
    | Double
    | String
    | Signature
    | Object_path
  type single =
    | Basic of basic
    | Array of single
    | Dict of basic * single
    | Structure of single list
    | Variant
  type t = single list
end

module type Exn = sig
  exception Fail of string
end

module SignatureReader(T : TypesSig)(E : Exn) : sig
  val read_t : string -> int -> int * T.t
    (** [read_t str pos] read a marshaled signature *)

  val read_single : string -> int -> int * T.single
    (** [read_single str pos] same as [read_t] but read only one
        single type *)
end

module SignatureWriter(T : TypesSig)(E : Exn) : sig
  val write_t : string -> int -> T.t -> int
    (** [write_t t str pos] write a signaute *)

  val write_single : string -> int -> T.single -> int
    (** [write_single t str pos] write a single type signaute *)
end
