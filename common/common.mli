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
  type typ =
    | Tbyte
    | Tboolean
    | Tint16
    | Tint32
    | Tint64
    | Tuint16
    | Tuint32
    | Tuint64
    | Tdouble
    | Tstring
    | Tsignature
    | Tobject_path
    | Tbasic of typ
    | Tarray of typ
    | Tdict of typ * typ
    | Tstructure of typ list
    | Tvariant
end

module type Exn = sig
  exception Fail of string
end

module SignatureReader(T : TypesSig)(E : Exn) : sig
  val read_list : string -> int -> int * T.typ list
    (** [read_list str pos] read a marshaled signature *)

  val read : string -> int -> int * T.typ
    (** [read str pos] same as [read_list] but read only one single
        type *)
end

module SignatureWriter(T : TypesSig)(E : Exn) : sig
  val write_list : string -> int -> T.typ list -> int
    (** [write_list t str pos] write a signaute *)

  val write : string -> int -> T.typ -> int
    (** [write t str pos] write a single type signaute *)
end
