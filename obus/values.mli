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

(** Marshaling (for auto-generated code) *)

open Wire

val dtype_signature_size : dtype -> int
val dtypes_signature_size : dtypes -> int
val read_dtype : buffer -> ptr -> dtype
val read_dtypes : buffer -> ptr -> dtypes
val write_dtype : buffer -> ptr -> dtype -> unit
val write_dtypes : buffer -> ptr -> dtypes -> unit

module type Reader = sig
  val read_value : buffer -> ptr -> dtype -> ptr * value
  val read_values : buffer -> ptr -> dtypes -> ptr * values
end

module type Writer = sig
  val write_value : buffer -> ptr -> value -> buffer * ptr
  val write_values : buffer -> ptr -> values -> buffer * ptr
end

module LEReader : Reader
module BEReader : Reader
module LEWriter : Writer
module BEWriter : Writer
