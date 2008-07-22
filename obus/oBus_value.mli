(*
 * oBus_value.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Dynamically typed DBus values *)

(** {6 Representation} *)

type basic =
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
  | Signature of OBus_types.signature
  | Object_path of OBus_path.t

type single =
    private
  | Basic of basic
  | Array of OBus_types.single * single list
  | Dict of OBus_types.basic * OBus_types.single * (basic * single) list
  | Struct of single list
  | Variant of single

type sequence = single list

(** {6 Construction} *)

val vbyte : char -> basic
val vboolean : bool -> basic
val vint16 : int -> basic
val vint32 : int32 -> basic
val vint64 : int64 -> basic
val vuint16 : int -> basic
val vuint32 : int32 -> basic
val vuint64 : int64 -> basic
val vdouble : float -> basic
val vstring : string -> basic
val vsignature : OBus_types.signature -> basic
val vobject_path : OBus_path.t -> basic

val vbasic : basic -> single
val varray : OBus_types.single -> single list -> single
val vdict : OBus_types.basic -> OBus_types.single -> (basic * single) list -> single
  (** [varray] and [vdict] raise an [Invalid_argument] if one of the
      value does not have the expected type *)
val vstruct : single list -> single
val vvariant : single -> single

(** {6 Manipulation} *)

val type_of_basic : basic -> OBus_types.basic
val type_of_single : single -> OBus_types.single
val type_of_sequence : sequence -> OBus_types.sequence
  (** Return the type of a value *)

val string_of_basic : basic -> string
val string_of_single : single -> string
val string_of_sequence : sequence -> string
  (** Pretty-printing *)

(** {6 Typed construction/deconstruction} *)

type ('a, 'cl) ty
  (** Description of a type, ['a] is the caml type used representing a
      DBus type and ['cl] is the class of the type *)

type cl_basic = [ `basic | `single | `sequence ]
type cl_single = [ `single | `sequence ]
type cl_sequence = [ `sequence ]
    (** Predicats on DBus types *)

val make_basic : ('a, [> `basic ]) ty -> 'a -> basic
val make_single : ('a, [> `single ]) ty -> 'a -> single
val make_sequence : ('a, [> `sequence ]) ty -> 'a -> sequence
  (** Create a DBus value from a standart caml value *)

exception Cast_failure

val cast_basic : ('a, [> `basic ]) ty -> basic -> 'a
val cast_single : ('a, [> `single ]) ty -> single -> 'a
val cast_sequence : ('a, [> `sequence ]) ty -> sequence -> 'a
  (** Cast a DBus value to a standart caml value. Raise a
      [Cast_failure] if types do not match *)

val opt_cast_basic : ('a, [> `basic ]) ty -> basic -> 'a option
val opt_cast_single : ('a, [> `single ]) ty -> single -> 'a option
val opt_cast_sequence : ('a, [> `sequence ]) ty -> sequence -> 'a option
  (** Same thing but return an option instead of raising an
      exception *)

val type_of_basic_ty : ('a, [> `basic ]) ty -> OBus_types.basic
val type_of_single_ty : ('a, [> `single ]) ty -> OBus_types.single
val type_of_sequence_ty : ('a, [> `sequence ]) ty -> OBus_types.sequence

type ('b, 'c) with_ty = { with_ty : 'a. ('a, 'b) ty -> 'c }
val with_basic_ty : (cl_basic, 'a) with_ty -> OBus_types.basic -> 'a
val with_single_ty : (cl_single, 'a) with_ty -> OBus_types.single -> 'a
val with_sequence_ty : (cl_sequence, 'a) with_ty -> OBus_types.sequence -> 'a
  (** Manipulate a typed representation of a type from an untyped one,
      this can be used to cast a DBus value without having to
      completely match its type *)

(** {6 Typed constructors} *)

val tbyte : (char, cl_basic) ty
val tboolean : (bool, cl_basic) ty
val tint16 : (int, cl_basic) ty
val tint32 : (int32, cl_basic) ty
val tint64 : (int64, cl_basic) ty
val tuint16 : (int, cl_basic) ty
val tuint32 : (int32, cl_basic) ty
val tuint64 : (int64, cl_basic) ty
val tdouble : (float, cl_basic) ty
val tstring : (string, cl_basic) ty
val tsignature : (OBus_types.signature, cl_basic) ty
val tobject_path : (OBus_path.t, cl_basic) ty

val tarray : ('a, [> `single ]) ty -> ('a list, cl_single) ty
val tdict : ('a, [> `basic ]) ty -> ('b, [> `single ]) ty -> (('a * 'b) list, cl_single) ty
val tstruct : ('a, [> `sequence ]) ty -> ('a, cl_single) ty
val tvariant : (single, cl_single) ty

val tcons : ('a, [> `single ]) ty -> ('b, [> `sequence ]) ty -> ('a * 'b, cl_sequence) ty
val tnil : (unit, cl_sequence) ty

val tup0 :
  (unit, cl_sequence) ty
val tup1 :
  ('a1, [> `single ]) ty ->
  ('a1, cl_sequence) ty
val tup2 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a1 * 'a2, cl_sequence) ty
val tup3 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a3, [> `single ]) ty ->
  ('a1 * 'a2 * 'a3, cl_sequence) ty
val tup4 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a3, [> `single ]) ty ->
  ('a4, [> `single ]) ty ->
  ('a1 * 'a2 * 'a3 * 'a4, cl_sequence) ty
val tup5 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a3, [> `single ]) ty ->
  ('a4, [> `single ]) ty ->
  ('a5, [> `single ]) ty ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5, cl_sequence) ty
val tup6 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a3, [> `single ]) ty ->
  ('a4, [> `single ]) ty ->
  ('a5, [> `single ]) ty ->
  ('a6, [> `single ]) ty ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6, cl_sequence) ty
val tup7 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a3, [> `single ]) ty ->
  ('a4, [> `single ]) ty ->
  ('a5, [> `single ]) ty ->
  ('a6, [> `single ]) ty ->
  ('a7, [> `single ]) ty ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7, cl_sequence) ty
val tup8 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a3, [> `single ]) ty ->
  ('a4, [> `single ]) ty ->
  ('a5, [> `single ]) ty ->
  ('a6, [> `single ]) ty ->
  ('a7, [> `single ]) ty ->
  ('a8, [> `single ]) ty ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8, cl_sequence) ty
val tup9 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a3, [> `single ]) ty ->
  ('a4, [> `single ]) ty ->
  ('a5, [> `single ]) ty ->
  ('a6, [> `single ]) ty ->
  ('a7, [> `single ]) ty ->
  ('a8, [> `single ]) ty ->
  ('a9, [> `single ]) ty ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9, cl_sequence) ty
val tup10 :
  ('a1, [> `single ]) ty ->
  ('a2, [> `single ]) ty ->
  ('a3, [> `single ]) ty ->
  ('a4, [> `single ]) ty ->
  ('a5, [> `single ]) ty ->
  ('a6, [> `single ]) ty ->
  ('a7, [> `single ]) ty ->
  ('a8, [> `single ]) ty ->
  ('a9, [> `single ]) ty ->
  ('a10, [> `single ]) ty ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10, cl_sequence) ty

(** Note that [tcons a (tcons b (tcons c (tcons d tnil)))], [tup4 a b
    c d], [tcons a (tup3 b c d)], ... represent the same DBus type *)
