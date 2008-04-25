(*
 * common.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Shared code *)

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
  | Tarray of typ
  | Tdict of typ * typ
  | Tstructure of typ list
  | Tvariant

module type Exn = sig
  exception Fail of string
end

module SignatureReader(E : Exn) : sig
  val read_list : string -> int -> int * typ list
    (** [read_list str pos] read a marshaled signature *)

  val read : string -> int -> int * typ
    (** [read str pos] same as [read_list] but read only one single
        type *)
end

module SignatureWriter(E : Exn) : sig
  val write_list : string -> int -> typ list -> int
    (** [write_list t str pos] write a signaute *)

  val write : string -> int -> typ -> int
    (** [write t str pos] write a single type signaute *)
end
