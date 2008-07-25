(*
 * oBus_types.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** DBus types *)

type basic =
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

type single =
  | Tbasic of basic
  | Tstruct of single list
  | Tarray of single
  | Tdict of basic * single
  | Tvariant

type sequence = single list

(** {6 Singatures} *)

type signature = sequence

val string_of_signature : signature -> string
  (** Return a string representation of a signature using DBus type
      codes *)

val signature_of_string : string -> signature
  (** Parse a signature, return an [Failure] if the signature is not
      correct *)

(** {6 Pretty-printing} *)

val string_of_basic : basic -> string
val string_of_single : single -> string
val string_of_sequence : sequence -> string

(** {6 Combinators (for the syntax extension)} *)

(*val ob_signature : (signature, _, dsignature) OBus_comb.one*)
