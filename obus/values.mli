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
  | Signature of dtype list
  | Object_path of string
  | Array of dtype * value list
      (** Array and dict must also contain types information because
          they can be empty *)
  | Dict of dtype * dtype * (value * value) list
  | Structure of value list
  | Variant of value
type values = value list

val string_of_value : value -> string
val string_of_values : value list -> string
val dtype_of_value : value -> dtype
val dtype_of_values : value list -> dtype list

(** {6 DBus types/values construction} *)

type ('a, 'is_basic) cstr
type yes
type no
type 'a seq_cstr

val byte : (char, yes) cstr
val boolean : (bool, yes) cstr
val int16 : (int, yes) cstr
val int32 : (int32, yes) cstr
val int64 : (int64, yes) cstr
val uint16 : (int, yes) cstr
val uint32 : (int32, yes) cstr
val uint64 : (int64, yes) cstr
val double : (float, yes) cstr
val string : (string, yes) cstr
val signature : (dtype list, yes) cstr
val object_path : (string, yes) cstr
val array : ('a, _) cstr -> ('a list, no) cstr
val dict : ('a, yes) cstr -> ('b, _) cstr -> (('a * 'b) list, no) cstr
val structure : 'a seq_cstr  -> ('a, no) cstr
val variant : (value, yes) cstr
val cons : ('a, _) cstr -> 'b seq_cstr -> ('a * 'b) seq_cstr
val nil : unit seq_cstr

val make_dtype : (_, _) cstr -> dtype
val make_value : ('a, _) cstr -> 'a -> value
val make_dtypes : 'a seq_cstr -> dtype list
val make_values : 'a seq_cstr -> 'a -> value list

val get_value : ('a, _) cstr -> value -> 'a
val get_values : 'a seq_cstr -> value list -> 'a
  (** [get] and [get_list] raise an [Invalid_argument] if the value
      has not the valid type *)

(** Marshaling (for auto-generated code) *)

val read_values : Header.recv -> Wire.buffer -> Wire.ptr -> value list
val write_values : value list -> Header.byte_order -> Wire.buffer -> Wire.ptr -> int

module Writer(W : Wire.Writer) : sig
  val dtype : Wire.ptr -> dtype -> Wire.ptr
  val dtypes : Wire.ptr -> dtypes -> Wire.ptr
  val value : Wire.ptr -> value -> Wire.ptr
  val values : Wire.ptr -> values -> Wire.ptr
end

module Reader(R : Wire.Reader) : sig
  val dtype : Wire.ptr -> Wire.ptr * dtype
  val dtypes : Wire.ptr -> Wire.ptr * dtypes
  val value : Wire.ptr -> dtype -> Wire.ptr * value
  val values : Wire.ptr -> dtypes -> Wire.ptr * values
end
