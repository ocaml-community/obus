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
  | Tstructure of tsingle list
  | Tarray of telement
  | Tvariant

and telement =
  | Tdict_entry of tbasic * tsingle
  | Tsingle of tsingle

type tsequence = tsingle list

(** {6 Singatures} *)

type signature = tsequence

val string_of_signature : signature -> string
  (** Return a string representation of a signature using DBus type
      codes *)

val signature_of_string : string -> signature
  (** Parse a signature, return an [Failure] if the signature is not
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

type single =
    private
  | Basic of basic
  | Array of telement * element list
  | Structure of single list
  | Variant of single

and element =
  | Dict_entry of basic * single
  | Single of single

type sequence = single list

(** {6 Constructors} *)

val byte : char -> basic
val boolean : bool -> basic
val int16 : int -> basic
val int32 : int32 -> basic
val int64 : int64 -> basic
val uint16 : int -> basic
val uint32 : int32 -> basic
val uint64 : int64 -> basic
val double : float -> basic
val string : string -> basic
val signature : signature -> basic
val object_path : OBus_path.t -> basic

val basic : basic -> single
val array : telement -> element list -> single
  (** [array] raise an [Invalid_argument] if one of the value does
      not have the expected type *)
val structure : single list -> single
val variant : single -> single

val dict_entry : basic -> single -> element
val single : single -> element

(** {6 Utils} *)

val type_of_basic : basic -> tbasic
val type_of_single : single -> tsingle
val type_of_element : element -> telement
val type_of_sequence : sequence -> tsequence
  (** Return the type of a value *)

val print_tbasic : Format.formatter -> tbasic -> unit
val print_tsingle : Format.formatter -> tsingle -> unit
val print_telement : Format.formatter -> telement -> unit
val print_tsequence : Format.formatter -> tsequence -> unit
val print_basic : Format.formatter -> basic -> unit
val print_single : Format.formatter -> single -> unit
val print_element : Format.formatter -> element -> unit
val print_sequence : Format.formatter -> sequence -> unit
  (** Pretty-printing *)

val string_of_tbasic : tbasic -> string
val string_of_tsingle : tsingle -> string
val string_of_telement : telement -> string
val string_of_tsequence : tsequence -> string
val string_of_basic : basic -> string
val string_of_single : single -> string
val string_of_element : element -> string
val string_of_sequence : sequence -> string
