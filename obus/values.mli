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

val byte : (char, _) cstr
val boolean : (bool, _) cstr
val int16 : (int, _) cstr
val int32 : (int32, _) cstr
val int64 : (int64, _) cstr
val uint16 : (int, _) cstr
val uint32 : (int32, _) cstr
val uint64 : (int64, _) cstr
val double : (float, _) cstr
val string : (string, _) cstr
val signature : (dtypes, _) cstr
val object_path : (string, _) cstr
val array : ('a, _) cstr -> ('a list, no) cstr
val dict : ('a, yes) cstr -> ('b, _) cstr -> (('a * 'b) list, no) cstr
val structure : 'a seq_cstr  -> ('a, no) cstr
val variant : (value, no) cstr
val cons : ('a, _) cstr -> 'b seq_cstr -> ('a * 'b) seq_cstr
val nil : unit seq_cstr

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

val vbyte : char -> value
val vboolean : bool -> value
val vint16 : int -> value
val vint32 : int32 -> value
val vint64 : int64 -> value
val vuint16 : int -> value
val vuint32 : int32 -> value
val vuint64 : int64 -> value
val vdouble : float -> value
val vstring : string -> value
val vsignature : dtypes -> value
val vobject_path : string -> value
val varray : dtype -> value list -> value
val vdict : dtype -> dtype -> (value * value) list -> value
val vstructure : values -> value
val vvariant : value -> value

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
