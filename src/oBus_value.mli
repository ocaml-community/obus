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
  | Tunix_fd

type tsingle =
  | Tbasic of tbasic
  | Tstructure of tsingle list
  | Tarray of tsingle
  | Tdict of tbasic * tsingle
  | Tvariant

type tsequence = tsingle list

(** {8 Constructors} *)

val tbyte : tbasic
val tboolean : tbasic
val tint16 : tbasic
val tint32 : tbasic
val tint64 : tbasic
val tuint16 : tbasic
val tuint32 : tbasic
val tuint64 : tbasic
val tdouble : tbasic
val tstring : tbasic
val tsignature : tbasic
val tobject_path : tbasic
val tunix_fd : tbasic

val tbasic : tbasic -> tsingle
val tstructure : tsingle list -> tsingle
val tarray : tsingle -> tsingle
val tdict : tbasic -> tsingle -> tsingle
val tvariant : tsingle

val tsbyte : tsingle
  (** [tsbyte = tbasic tbyte] *)
val tsboolean : tsingle
  (** [tsboolean = tbasic tboolean] *)
val tsint16 : tsingle
  (** [tsint16 = tbasic tint16] *)
val tsint32 : tsingle
  (** [tsint32 = tbasic tint32] *)
val tsint64 : tsingle
  (** [tsint64 = tbasic tint64] *)
val tsuint16 : tsingle
  (** [tsuint16 = tbasic tuint16] *)
val tsuint32 : tsingle
  (** [tsuint32 = tbasic tuint32] *)
val tsuint64 : tsingle
  (** [tsuint64 = tbasic tuint64] *)
val tsdouble : tsingle
  (** [tsdouble = tbasic tdouble] *)
val tsstring : tsingle
  (** [tsstring = tbasic tstring] *)
val tssignature : tsingle
  (** [tssignature = tbasic tsignature] *)
val tsobject_path : tsingle
  (** [tsobject_path = tbasic tobject_path] *)
val tsunix_fd : tsingle
  (** [tsunix_fd = tbasic tunix_fd] *)

(** {6 Singatures} *)

type signature = tsequence

exception Invalid_signature of string * string
  (** [Invalid_signature(signature, message)] is raised when a
      signature is invalid. [signature] is a string representation of
      the signature (using D-Bus type codes) and [message] is an error
      message. *)

val string_of_signature : signature -> string
  (** Returns a string representation of a signature using DBus type
      codes. If the signature is not valid (for example it is too
      long), it raises {!Invalid_signature}. *)

val signature_of_string : string -> signature
  (** Parses a signature. Raises {!Invalid_signature} if the signature
      is not correct *)

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
  | Unix_fd of Unix.file_descr

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

type sequence = single list

(** {8 Constructors} *)

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
val unix_fd : Unix.file_descr -> basic

val basic : basic -> single
val array : tsingle -> single list -> single
val byte_array : string -> single
val dict : tbasic -> tsingle -> (basic * single) list -> single
val structure : single list -> single
val variant : single -> single

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
val sunix_fd : Unix.file_descr -> single
  (** [sunix_fd x = basic (unix_fd x)] *)

(** {6 Typing} *)

val type_of_basic : basic -> tbasic
val type_of_single : single -> tsingle
val type_of_sequence : sequence -> tsequence
  (** Return the type of a value *)

(** {6 Printing} *)

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

(** {6 File descriptors utils} *)

val basic_dup : basic -> basic
val single_dup : single -> single
val sequence_dup : sequence -> sequence
  (** Duplicates all file descriptors of the given value *)

val basic_close : basic -> unit
val single_close : single -> unit
val sequence_close : sequence -> unit
  (** Closes all file descriptors of the given value *)
