(*
 * oBus_value.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus types, values and converters *)

(** {6 Types} *)

(** D-Bus types *)
module T : sig

  type basic =
    | Byte
    | Boolean
    | Int16
    | Int32
    | Int64
    | Uint16
    | Uint32
    | Uint64
    | Double
    | String
    | Signature
    | Object_path
    | Unix_fd

  type single =
    | Basic of basic
    | Structure of single list
    | Array of single
    | Dict of basic * single
    | Variant

  type sequence = single list

  (** {6 Constructors} *)

  val byte : basic
  val boolean : basic
  val int16 : basic
  val int32 : basic
  val int64 : basic
  val uint16 : basic
  val uint32 : basic
  val uint64 : basic
  val double : basic
  val string : basic
  val signature : basic
  val object_path : basic
  val unix_fd : basic

  val basic : basic -> single
  val structure : single list -> single
  val array : single -> single
  val dict : basic -> single -> single
  val variant : single

  val basic_byte : single
  val basic_boolean : single
  val basic_int16 : single
  val basic_int32 : single
  val basic_int64 : single
  val basic_uint16 : single
  val basic_uint32 : single
  val basic_uint64 : single
  val basic_double : single
  val basic_string : single
  val basic_signature : single
  val basic_object_path : single
  val basic_unix_fd : single

  (** {6 Pretty printing} *)

  val print_basic : Format.formatter -> basic -> unit
  val print_single : Format.formatter -> single -> unit
  val print_sequence : Format.formatter -> sequence -> unit

  val string_of_basic : basic -> string
  val string_of_single : single -> string
  val string_of_sequence : sequence -> string
end

(** {6 Signatures} *)

type signature = T.sequence

exception Invalid_signature of string * string
  (** [Invalid_signature(signature, message)] is raised when a
      signature is invalid. [signature] is a string representation of
      the signature (using D-Bus type codes) and [message] is an error
      message. *)

val string_of_signature : signature -> string
  (** Returns a string representation of a signature using D-Bus type
      codes. If the signature is not valid (for example it is too
      long), it raises {!Invalid_signature}. *)

val signature_of_string : string -> signature
  (** Parses a signature. Raises {!Invalid_signature} if the signature
      is not correct *)

val validate_signature : signature -> string option
  (** Not all signatures are valid. [validate] returns [None] if the
      given signature is a valid one, or [Some reason] if it is
      not. *)

(** {6 Values} *)

(** D-Bus values *)
module V : sig

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
    | Array of T.single * single list
    | Byte_array of string
    | Dict of T.basic * T.single * (basic * single) list
    | Structure of single list
    | Variant of single

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
  val unix_fd : Unix.file_descr -> basic

  val basic : basic -> single
  val array : T.single -> single list -> single
  val byte_array : string -> single
  val dict : T.basic -> T.single -> (basic * single) list -> single
  val structure : single list -> single
  val variant : single -> single

  (**/**)

  val unsafe_array : T.single -> single list -> single
  val unsafe_dict : T.basic -> T.single -> (basic * single) list -> single

  (**/**)

  val basic_byte : char -> single
  val basic_boolean : bool -> single
  val basic_int16 : int -> single
  val basic_int32 : int32 -> single
  val basic_int64 : int64 -> single
  val basic_uint16 : int -> single
  val basic_uint32 : int32 -> single
  val basic_uint64 : int64 -> single
  val basic_double : float -> single
  val basic_string : string -> single
  val basic_signature : signature -> single
  val basic_object_path : OBus_path.t -> single
  val basic_unix_fd : Unix.file_descr -> single

  (** {6 Typing} *)

  val type_of_basic : basic -> T.basic
  val type_of_single : single -> T.single
  val type_of_sequence : sequence -> T.sequence

  (** {6 Pretty printing} *)

  val print_basic : Format.formatter -> basic -> unit
  val print_single : Format.formatter -> single -> unit
  val print_sequence : Format.formatter -> sequence -> unit

  val string_of_basic : basic -> string
  val string_of_single : single -> string
  val string_of_sequence : sequence -> string

  (** {6 File descriptors utils} *)

  val basic_dup : basic -> basic
  val single_dup : single -> single
  val sequence_dup : sequence -> sequence
    (** Duplicates all file descriptors of the given value *)

  val basic_close : basic -> unit Lwt.t
  val single_close : single -> unit Lwt.t
  val sequence_close : sequence -> unit Lwt.t
    (** Closes all file descriptors of the given value *)
end

(** {6 Type converters} *)

(** Type converters *)
module C : sig

  (** This module offers a convenient way of constructing a boxed D-Bus
      value from a OCaml value, and of casting a boxed D-Bus value
      into a OCaml value. *)

  type 'a basic
    (** Type of converters dealing with basic D-Bus types *)

  type 'a single
    (** Type of converters dealing with single D-Bus types *)

  type 'a sequence
    (** Type of converters dealing with sequence D-Bus types *)

  (** {6 Constructors} *)

  val byte : char basic
  val boolean : bool basic
  val int16 : int basic
  val int32 : int32 basic
  val int64 : int64 basic
  val uint16 : int basic
  val uint32 : int32 basic
  val uint64 : int64 basic
  val double : float basic
  val string : string basic
  val signature : signature basic
  val object_path : OBus_path.t basic
  val unix_fd : Unix.file_descr basic

  val basic : 'a basic -> 'a single
  val structure : 'a sequence -> 'a single
  val byte_array : string single
  val array : 'a single -> 'a list single
  val dict : 'a basic -> 'b single -> ('a * 'b) list single
  val variant : V.single single

  val basic_byte : char single
  val basic_boolean : bool single
  val basic_int16 : int single
  val basic_int32 : int32 single
  val basic_int64 : int64 single
  val basic_uint16 : int single
  val basic_uint32 : int32 single
  val basic_uint64 : int64 single
  val basic_double : float single
  val basic_string : string single
  val basic_signature : signature single
  val basic_object_path : OBus_path.t single
  val basic_unix_fd : Unix.file_descr single

  (** {6 Types extraction} *)

  val type_basic : 'a basic -> T.basic
  val type_single : 'a single -> T.single
  val type_sequence : 'a sequence -> T.sequence

  (** {6 Boxing} *)

  val make_basic : 'a basic -> 'a -> V.basic
  val make_single : 'a single -> 'a -> V.single
  val make_sequence : 'a sequence -> 'a -> V.sequence

  (**  {6 Unboxing} *)

  exception Signature_mismatch
    (** Exception raised when a boxed value do not have the same
        signature as the combinator *)

  val cast_basic : 'a basic -> V.basic -> 'a
  val cast_single : 'a single -> V.single -> 'a
  val cast_sequence : 'a sequence -> V.sequence -> 'a

  (** {6 Dynamic values} *)

  (** The follwing functions allows you to create converters that do
      not convert values. *)

  val dyn_basic : T.basic -> V.basic basic
  val dyn_single : T.single -> V.single single
  val dyn_sequence : T.sequence -> V.sequence sequence

  (** {6 Sequence constructors} *)

  val seq0 : unit sequence
  val seq1 : 'a1 single -> 'a1 sequence
  val seq2 : 'a1 single -> 'a2 single -> ('a1 * 'a2) sequence
  val seq3 : 'a1 single -> 'a2 single -> 'a3 single -> ('a1 * 'a2 * 'a3) sequence
  val seq4 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> ('a1 * 'a2 * 'a3 * 'a4) sequence
  val seq5 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) sequence
  val seq6 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) sequence
  val seq7 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) sequence
  val seq8 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) sequence
  val seq9 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> 'a9 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) sequence
  val seq10 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> 'a9 single -> 'a10 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) sequence
  val seq11 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> 'a9 single -> 'a10 single -> 'a11 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11) sequence
  val seq12 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> 'a9 single -> 'a10 single -> 'a11 single -> 'a12 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12) sequence
  val seq13 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> 'a9 single -> 'a10 single -> 'a11 single -> 'a12 single -> 'a13 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12 * 'a13) sequence
  val seq14 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> 'a9 single -> 'a10 single -> 'a11 single -> 'a12 single -> 'a13 single -> 'a14 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12 * 'a13 * 'a14) sequence
  val seq15 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> 'a9 single -> 'a10 single -> 'a11 single -> 'a12 single -> 'a13 single -> 'a14 single -> 'a15 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12 * 'a13 * 'a14 * 'a15) sequence
  val seq16 : 'a1 single -> 'a2 single -> 'a3 single -> 'a4 single -> 'a5 single -> 'a6 single -> 'a7 single -> 'a8 single -> 'a9 single -> 'a10 single -> 'a11 single -> 'a12 single -> 'a13 single -> 'a14 single -> 'a15 single -> 'a16 single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12 * 'a13 * 'a14 * 'a15 * 'a16) sequence
end

(** {6 Methods/signals arguments} *)

(** Type of a list of arguments used by methods and signals. It is
    ensured that the number of single types contained in [arg_types]
    is equal to the number of names. *)
type 'a arguments = private {
  arg_types : 'a C.sequence;
  (** Types of the arguments *)
  arg_names : string option list;
  (** Names of the arguments *)
}

val arguments : arg_types : 'a C.sequence -> arg_names : string option list -> 'a arguments
  (** [arguments ~arg_types ~arg_names] creates a list of
      arguments. It raises [Invalid_arg] if the number of single types
      contained in [arg_types] is not equal to the number of names. *)

val arg_types : 'a arguments -> 'a C.sequence
  (** Returns the underlying sequence converter of a list of
      arguments. *)

val arg_names : 'a arguments -> string option list
  (** Returns the names of a list of arguments *)

(** {8 Constructors} *)

val arg_cons : string option * 'a C.single -> 'b arguments -> ('a * 'b) arguments
  (** [arg_cons (name, typ) arguments] adds the argument [(name,
      type)] to the beginning of [arguments] *)

val arg0 : unit arguments
val arg1 : string option * 'a1 C.single -> 'a1 arguments
val arg2 : string option * 'a1 C.single -> string option * 'a2 C.single -> ('a1 * 'a2) arguments
val arg3 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> ('a1 * 'a2 * 'a3) arguments
val arg4 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> ('a1 * 'a2 * 'a3 * 'a4) arguments
val arg5 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) arguments
val arg6 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) arguments
val arg7 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) arguments
val arg8 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) arguments
val arg9 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> string option * 'a9 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) arguments
val arg10 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> string option * 'a9 C.single -> string option * 'a10 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) arguments
val arg11 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> string option * 'a9 C.single -> string option * 'a10 C.single -> string option * 'a11 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11) arguments
val arg12 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> string option * 'a9 C.single -> string option * 'a10 C.single -> string option * 'a11 C.single -> string option * 'a12 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12) arguments
val arg13 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> string option * 'a9 C.single -> string option * 'a10 C.single -> string option * 'a11 C.single -> string option * 'a12 C.single -> string option * 'a13 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12 * 'a13) arguments
val arg14 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> string option * 'a9 C.single -> string option * 'a10 C.single -> string option * 'a11 C.single -> string option * 'a12 C.single -> string option * 'a13 C.single -> string option * 'a14 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12 * 'a13 * 'a14) arguments
val arg15 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> string option * 'a9 C.single -> string option * 'a10 C.single -> string option * 'a11 C.single -> string option * 'a12 C.single -> string option * 'a13 C.single -> string option * 'a14 C.single -> string option * 'a15 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12 * 'a13 * 'a14 * 'a15) arguments
val arg16 : string option * 'a1 C.single -> string option * 'a2 C.single -> string option * 'a3 C.single -> string option * 'a4 C.single -> string option * 'a5 C.single -> string option * 'a6 C.single -> string option * 'a7 C.single -> string option * 'a8 C.single -> string option * 'a9 C.single -> string option * 'a10 C.single -> string option * 'a11 C.single -> string option * 'a12 C.single -> string option * 'a13 C.single -> string option * 'a14 C.single -> string option * 'a15 C.single -> string option * 'a16 C.single -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12 * 'a13 * 'a14 * 'a15 * 'a16) arguments
