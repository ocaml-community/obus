(*
 * values.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module T : Types.S

(** Representation of DBus values *)

module type S = sig
  type basic =
      private
    | Byte of char
    | Boolean of bool
    | Int16 of int
    | Int32 of int32
    | Int64 of int64
    | Uint16 of int
    | Uint32 of int32
    | Uint64 of int64
    | Double of float
    | String of string
    | Signature of T.t
    | Object_path of string

  type single =
      private
    | Basic of basic
    | Array of T.single * single list
        (** Array and dict must also contain types information because
            they can be empty *)
    | Dict of T.basic * T.single * (basic * single) list
    | Structure of single list
    | Variant of single

  type t = single list

  val type_of_basic : basic -> T.basic
  val type_of_single : single -> T.single
  val type_of_t : t -> T.t

  val string_of_basic : basic -> string
  val string_of_single : single -> string
  val string_of_t : t -> string

  type 'a tbasic
  type 'a tsingle
  type ('a, 'b) tlist

  val byte : char -> T.byte tbasic
  val boolean : bool -> T.boolean tbasic
  val int16 : int -> T.int16 tbasic
  val int32 : int32 -> T.int32 tbasic
  val int64 : int64 -> T.int64 tbasic
  val uint16 : int -> T.uint16 tbasic
  val uint32 : int32 -> T.uint32 tbasic
  val uint64 : int64 -> T.uint64 tbasic
  val double : float -> T.double tbasic
  val string : string -> T.string tbasic
  val signature : T.t -> T.signature tbasic
  val object_path : string -> T.object_path tbasic
  val basic : 'a tbasic -> 'a tsingle
  val array : 'a T.tsingle -> 'a tsingle list -> 'a T.array tsingle
  val dict : 'a T.tbasic -> 'b T.tsingle -> ('a tbasic * 'b tsingle) list -> ('a, 'b) T.dict tsingle
  val structure : ('a, T.nil) tlist -> 'a T.structure tsingle
  val variant : 'a tsingle -> T.variant tsingle
  val cons : 'a tsingle -> ('b, 'c) tlist -> ('a * 'b, 'c) tlist
  val nil : ('a, 'a) tlist
  val concat : ('a, 'b) tlist -> ('b, 'c) tlist -> ('a, 'c) tlist

  val basic_of_tbasic : 'a tbasic -> basic
  val single_of_tsingle : 'a tsingle -> single
  val list_of_tlist : ('a, 'b) tlist -> t

  val type_of_tbasic : 'a tbasic -> 'a T.tbasic
  val type_of_tsingle : 'a tsingle -> 'a T.tsingle
  val type_of_tlist : ('a, 'b) tlist -> ('a, 'b) T.tlist
end

module M : S
