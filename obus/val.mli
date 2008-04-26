(*
 * type.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** {6 DBus types} *)

type typ =
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
  | Tarray of typ
  | Tdict of typ * typ
  | Tstructure of typ list
  | Tvariant

val string_of_type : typ -> string
val string_of_types : typ list -> string

val signature_of_type : typ -> string
val signature_of_types : typ list -> string

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
  | Signature of typ list
  | Object_path of string
  | Array of typ * value list
      (** Array and dict must also contain types information because
          they can be empty *)
  | Dict of typ * typ * (value * value) list
  | Structure of value list
  | Variant of value

val string_of_value : value -> string
val string_of_values : value list -> string
val type_of_value : value -> typ
val type_of_values : value list -> typ list

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
val signature : (typ list, yes) cstr
val object_path : (string, yes) cstr
val array : ('a, _) cstr -> ('a list, no) cstr
val dict : ('a, yes) cstr -> ('b, _) cstr -> (('a * 'b) list, no) cstr
val structure : 'a seq_cstr  -> ('a, no) cstr
val variant : (value, yes) cstr
val cons : ('a, _) cstr -> 'b seq_cstr -> ('a * 'b) seq_cstr
val nil : unit seq_cstr

val make_type : (_, _) cstr -> typ
val make_value : ('a, _) cstr -> 'a -> value
val make_type_list : 'a seq_cstr -> typ list
val make_value_list : 'a seq_cstr -> 'a -> value list

val get : ('a, _) cstr -> value -> 'a
val get_list : 'a seq_cstr -> value list -> 'a
  (** [get] and [get_list] raise an [Invalid_argument] if the value
      has not the valid type *)

(** Marshaling (for auto-generated code) *)

val read_value : Header.t -> Wire.buffer -> Wire.ptr -> value list
val write_value : value list -> Header.byte_order -> Wire.buffer -> Wire.ptr -> int

module Writer(W : Wire.Writer) : sig
  val typ : Wire.ptr -> typ -> Wire.ptr
  val typ_list : Wire.ptr -> typ list -> Wire.ptr
  val value : Wire.ptr -> value -> Wire.ptr
  val value_list : Wire.ptr -> value list -> Wire.ptr
end

module Reader(R : Wire.Reader) : sig
  val typ : Wire.ptr -> Wire.ptr * typ
  val typ_list : Wire.ptr -> Wire.ptr * typ list
  val value : Wire.ptr -> typ -> Wire.ptr * value
  val value_list : Wire.ptr -> typ list -> Wire.ptr * value list
end
