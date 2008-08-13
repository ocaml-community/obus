(*
 * oBus_value.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus types and values *)

(** {6 DBus types} *)

type tbasic =
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

type tsingle =
  | Tbasic of tbasic
  | Tstruct of tsingle list
  | Tarray of tsingle
  | Tdict of tbasic * tsingle
  | Tvariant

type tsequence = tsingle list

(** {6 Singatures} *)

type signature = tsequence

val string_of_signature : signature -> string
  (** Return a string representation of a signature using DBus type
      codes *)

val signature_of_string : string -> signature
  (** Parse a signature, return an [Failure] if the signature is not
      correct *)

(** {6 DBus values} *)

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
  | Signature of signature
  | Object_path of OBus_path.t

type single =
    private
  | Basic of basic
  | Array of tsingle * single list
  | Dict of tbasic * tsingle * (basic * single) list
  | Struct of single list
  | Variant of single

type sequence = single list

(** {6 Constructors} *)

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
val vsignature : signature -> basic
val vobject_path : OBus_path.t -> basic

val vbasic : basic -> single
val varray : tsingle -> single list -> single
val vdict : tbasic -> tsingle -> (basic * single) list -> single
  (** [varray] and [vdict] raise an [Invalid_argument] if one of the
      value does not have the expected type *)
val vstruct : single list -> single
val vvariant : single -> single

(** {6 Utils} *)

val type_of_basic : basic -> tbasic
val type_of_single : single -> tsingle
val type_of_sequence : sequence -> tsequence
  (** Return the type of a value *)

val string_of_tbasic : tbasic -> string
val string_of_tsingle : tsingle -> string
val string_of_tsequence : tsequence -> string
val string_of_basic : basic -> string
val string_of_single : single -> string
val string_of_sequence : sequence -> string
  (** Pretty-printing *)
