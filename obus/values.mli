(*
 * values.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** {6 DBus types} *)

type dtype =
    private
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
  | Tarray of dtype
  | Tdict of dtype * dtype
  | Tstructure of dtype list
  | Tvariant
type dtypes = dtype list

val string_of_dtype : dtype -> string
val string_of_dtypes : dtypes -> string

val signature_of_dtype : dtype -> string
val dtype_of_signature : string -> dtype
val signature_of_dtypes : dtypes -> string
val dtypes_of_signature : string -> dtypes

(** {6 DBus values} *)

type value =
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
  | Signature of dtypes
  | Object_path of string
  | Array of dtype * value list
      (** Array and dict must also contain types information because
          they can be empty *)
  | Dict of dtype * dtype * (value * value) list
  | Structure of value list
  | Variant of value
type values = value list

val string_of_value : value -> string
val string_of_values : values -> string
val dtype_of_value : value -> dtype
val dtypes_of_values : values -> dtypes

(** {6 DBus types/values construction} *)

(** The following functions allow to create well formed dbus
    values. i.e. all element of arrays and dictionnaries have the same
    type, and key types of dictionaries are basic types *)

type ('a, 'is_basic) cstr
type yes
type no
type 'a seq_cstr

val cbyte : (char, _) cstr
val cboolean : (bool, _) cstr
val cint16 : (int, _) cstr
val cint32 : (int32, _) cstr
val cint64 : (int64, _) cstr
val cuint16 : (int, _) cstr
val cuint32 : (int32, _) cstr
val cuint64 : (int64, _) cstr
val cdouble : (float, _) cstr
val cstring : (string, _) cstr
val csignature : (dtypes, _) cstr
val cobject_path : (string, _) cstr
val carray : ('a, _) cstr -> ('a list, no) cstr
val cdict : ('a, yes) cstr -> ('b, _) cstr -> (('a * 'b) list, no) cstr
val cstructure : 'a seq_cstr  -> ('a, no) cstr
val cvariant : (value, no) cstr
val ccons : ('a, _) cstr -> 'b seq_cstr -> ('a * 'b) seq_cstr
val cnil : unit seq_cstr

val make_dtype : (_, _) cstr -> dtype
val make_value : ('a, _) cstr -> 'a -> value
val make_dtypes : _ seq_cstr -> dtypes
val make_values : 'a seq_cstr -> 'a -> values

val get_value : ('a, _) cstr -> value -> 'a
val get_values : 'a seq_cstr -> values -> 'a
  (** [get] and [get_list] raise an [Invalid_argument] if the value
      has not the valid type *)

(** {6 Alternatives constructions} *)

(** You can also use the following functions to create values and
    types. They raise an [Invalid_argument] if you try to construct
    incorrect values *)

val tbyte : dtype
val tboolean : dtype
val tint16 : dtype
val tint32 : dtype
val tint64 : dtype
val tuint16 : dtype
val tuint32 : dtype
val tuint64 : dtype
val tdouble : dtype
val tstring : dtype
val tsignature : dtype
val tobject_path : dtype
val tarray : dtype -> dtype
val tdict : dtype -> dtype -> dtype
val tstructure : dtypes -> dtype
val tvariant : dtype

val byte : char -> value
val boolean : bool -> value
val int16 : int -> value
val int32 : int32 -> value
val int64 : int64 -> value
val uint16 : int -> value
val uint32 : int32 -> value
val uint64 : int64 -> value
val double : float -> value
val string : string -> value
val signature : dtypes -> value
val object_path : string -> value
val array : dtype -> value list -> value
val dict : dtype -> dtype -> (value * value) list -> value
val structure : values -> value
val variant : value -> value

(**/**)

open Wire

val dtype_signature_size : dtype -> int
val dtypes_signature_size : dtypes -> int
val read_dtype : dtype reader
val read_dtypes : dtypes reader
val write_dtype : dtype writer
val write_dtypes : dtypes writer

module type Reader = sig
  val read_variant : value reader
  val read_value : dtype -> value reader
  val read_values : dtypes -> values reader
end

module type Writer = sig
  val write_variant : value writer
  val write_value : value writer
  val write_values : values writer
end

module LEReader : Reader
module BEReader : Reader
module LEWriter : Writer
module BEWriter : Writer
