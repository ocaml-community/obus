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

exception Cast_failure
  (** Raised when a cast fail *)

class type ['a] ty_sequence = object
  method make_sequence : 'a -> sequence
  method cast_sequence : sequence -> 'a
  method type_sequence : OBus_types.sequence
end

class virtual ['a] ty_single : object
  inherit ['a] ty_sequence
    (** One single type can is seen as a sequence of exactly one
        type *)

  method virtual make_single : 'a -> single
  method virtual cast_single : single -> 'a
  method virtual type_single : OBus_types.single
end

class virtual ['a] ty_basic : object
  inherit ['a] ty_single
    (** A basic type is a single type *)

  method virtual make_basic : 'a -> basic
  method virtual cast_basic : basic -> 'a
  method virtual type_basic : OBus_types.basic
end

val make_basic : 'a #ty_basic -> 'a -> basic
val make_single : 'a #ty_single -> 'a -> single
val make_sequence : 'a #ty_sequence -> 'a -> sequence
  (** Create a DBus value from a standart caml value *)

val cast_basic : 'a #ty_basic -> basic -> 'a
val cast_single : 'a #ty_single -> single -> 'a
val cast_sequence : 'a #ty_sequence -> sequence -> 'a
  (** Cast a DBus value to a standart caml value. Raise a
      [Cast_failure] if types do not match *)

val opt_cast_basic : 'a #ty_basic -> basic -> 'a option
val opt_cast_single : 'a #ty_single -> single -> 'a option
val opt_cast_sequence : 'a #ty_sequence -> sequence -> 'a option
  (** Same thing but return an option instead of raising an
      exception *)

val type_of_basic_ty : 'a #ty_basic -> OBus_types.basic
val type_of_single_ty : 'a #ty_single -> OBus_types.single
val type_of_sequence_ty : 'a #ty_sequence -> OBus_types.sequence

type 'a with_basic_ty = { with_basic_ty : 'b. 'b ty_basic -> 'a }
type 'a with_single_ty = { with_single_ty : 'b. 'b ty_single -> 'a }
type 'a with_sequence_ty = { with_sequence_ty : 'b. 'b ty_sequence -> 'a }
val with_basic_ty : 'a with_basic_ty -> OBus_types.basic -> 'a
val with_single_ty : 'a with_single_ty -> OBus_types.single -> 'a
val with_sequence_ty : 'a with_sequence_ty -> OBus_types.sequence -> 'a
  (** Manipulate a typed representation of a type from an untyped one,
      this can be used to cast a DBus value without having to
      completely match its type *)

(** {6 Typed constructors} *)

val tbyte : char ty_basic
val tboolean : bool ty_basic
val tint16 : int ty_basic
val tint32 : int32 ty_basic
val tint64 : int64 ty_basic
val tuint16 : int ty_basic
val tuint32 : int32 ty_basic
val tuint64 : int64 ty_basic
val tdouble : float ty_basic
val tstring : string ty_basic
val tsignature : OBus_types.signature ty_basic
val tobject_path : OBus_path.t ty_basic

val tarray : 'a #ty_single -> 'a list ty_single
val tdict : 'a #ty_basic -> 'b #ty_single -> ('a * 'b) list ty_single
val tstruct : 'a #ty_sequence -> 'a ty_single
val tvariant : single ty_single

val tcons : 'a #ty_single -> 'b #ty_sequence -> ('a * 'b) ty_sequence
val tnil : unit ty_sequence

val tup0 :
  unit ty_sequence
val tup1 :
  'a1 #ty_single ->
  'a1 ty_sequence
val tup2 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  ('a1 * 'a2) ty_sequence
val tup3 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  'a3 #ty_single ->
  ('a1 * 'a2 * 'a3) ty_sequence
val tup4 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  'a3 #ty_single ->
  'a4 #ty_single ->
  ('a1 * 'a2 * 'a3 * 'a4) ty_sequence
val tup5 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  'a3 #ty_single ->
  'a4 #ty_single ->
  'a5 #ty_single ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5) ty_sequence
val tup6 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  'a3 #ty_single ->
  'a4 #ty_single ->
  'a5 #ty_single ->
  'a6 #ty_single ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) ty_sequence
val tup7 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  'a3 #ty_single ->
  'a4 #ty_single ->
  'a5 #ty_single ->
  'a6 #ty_single ->
  'a7 #ty_single ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) ty_sequence
val tup8 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  'a3 #ty_single ->
  'a4 #ty_single ->
  'a5 #ty_single ->
  'a6 #ty_single ->
  'a7 #ty_single ->
  'a8 #ty_single ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) ty_sequence
val tup9 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  'a3 #ty_single ->
  'a4 #ty_single ->
  'a5 #ty_single ->
  'a6 #ty_single ->
  'a7 #ty_single ->
  'a8 #ty_single ->
  'a9 #ty_single ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) ty_sequence
val tup10 :
  'a1 #ty_single ->
  'a2 #ty_single ->
  'a3 #ty_single ->
  'a4 #ty_single ->
  'a5 #ty_single ->
  'a6 #ty_single ->
  'a7 #ty_single ->
  'a8 #ty_single ->
  'a9 #ty_single ->
  'a10 #ty_single ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) ty_sequence

(** Note that [tcons a (tcons b (tcons c (tcons d tnil)))], [tup4 a b
    c d], [tcons a (tup3 b c d)], ... represent the same DBus type *)

(** {6 Combinators (for the syntax extension)} *)

val ob_single : (single, _, OBus_types.dvariant) OBus_comb.one
