(*
 * oBus_value.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
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
 with constructor

type tsingle =
  | Tbasic of tbasic
  | Tstructure of tsingle list
  | Tarray of tsingle
  | Tdict of tbasic * tsingle
  | Tvariant
 with constructor

type tsequence = tsingle list

(** {6 Singatures} *)

type signature = tsequence

val string_of_signature : signature -> string
  (** Returns a string representation of a signature using DBus type
      codes *)

val signature_of_string : string -> signature
  (** Parses a signature, raises [Failure] if the signature is not
      correct *)

val validate_signature : signature -> string option
  (** Not all signatures are valid. [validate] returns [None] if the
      given signature is a valid one, or [Some reason] if it is
      not. *)

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
 with constructor

type single =
    private
  | Basic of basic
  | Array of tsingle * single list
  | Byte_array of string
      (** Array of bytes are always represented by a string in order
          to use less memory *)
  | Dict of tbasic * tsingle * (basic * single) list
      (** [array] and [dict] raise [Invalid_argument] if one of the
          value does not have the expected type *)
  | Structure of single list
  | Variant of single
 with constructor

type sequence = single list

val sbyte : char -> single
  (** [sbyte x = basic (byte x)] *)

val sboolean : bool -> single
  (** [sboolean x = basic (boolean x)] *)

val sint16 : int -> single
  (** [sint16 x = basic (int16 x)] *)

val sint32 : int32 -> single
  (** [sint32 x = basic (int32 x)] *)

val sint64 : int64 -> single
  (** [sint64 x = basic (int64 x)] *)

val suint16 : int -> single
  (** [suint16 x = basic (uint16 x)] *)

val suint32 : int32 -> single
  (** [suint32 x = basic (uint32 x)] *)

val suint64 : int64 -> single
  (** [suint64 x = basic (uint64 x)] *)

val sdouble : float -> single
  (** [sdouble x = basic (double x)] *)

val sstring : string -> single
  (** [sstring x = basic (string x)] *)

val ssignature : signature -> single
  (** [ssignature x = basic (signature x)] *)

val sobject_path : OBus_path.t -> single
  (** [sobject_path x = basic (object_path x)] *)

(** {6 OBus_utils} *)

val type_of_basic : basic -> tbasic
val type_of_single : single -> tsingle
val type_of_sequence : sequence -> tsequence
  (** Return the type of a value *)

val print_tbasic : Format.formatter -> tbasic -> unit
val print_tsingle : Format.formatter -> tsingle -> unit
val print_tsequence : Format.formatter -> tsequence -> unit
val print_basic : Format.formatter -> basic -> unit
val print_single : Format.formatter -> single -> unit
val print_sequence : Format.formatter -> sequence -> unit
  (** Pretty-printing *)

val string_of_tbasic : tbasic -> string
val string_of_tsingle : tsingle -> string
val string_of_tsequence : tsequence -> string
val string_of_basic : basic -> string
val string_of_single : single -> string
val string_of_sequence : sequence -> string
