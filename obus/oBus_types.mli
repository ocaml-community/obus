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

(** {6 Type annotations} *)

(** Type annotations are used to put in the type of monads or type
    combinators the DBus type information. This is essentially used
    for consistency reason. *)

type (+'a, +'b) annot
  (** ['b] is the DBus type structure and ['a] is the tail
      variable:

      For example the sequence (INT32, STRING, BYTE ARRAY) is encoded
      like that:

      [('a, dint32 * (dstring * (dbyte darray * 'a))) annot]

      ['a] is used to have a canonical representation of sequence, for
      example to identify the following types:

      [(dint32 * dint32) * dstring] and [dint32 * dint32 * dstring]
  *)

type (+'a, +'b) sannot = ('a, 'b * 'a) annot
    (** Annotation for a single type *)

(** {8 Types structure} *)

type dbyte = [`byte]
type dboolean = [`boolean]
type dint16 = [`int16]
type dint32 = [`int32]
type dint64 = [`int64]
type duint16 = [`uint16]
type duint32 = [`uint32]
type duint64 = [`uint64]
type ddouble = [`double]
type dstring = [`string]
type dsignature = [`signature]
type dobject_path = [`object_path]

type dbasic =
    [ dbyte
    | dboolean
    | dint16
    | dint32
    | dint64
    | duint16
    | duint32
    | duint64
    | ddouble
    | dstring
    | dsignature
    | dobject_path ]

type 'a dstruct
type 'a darray
type ('a, 'b) ddict
constraint 'a = [< dbasic ]
type dvariant

(** {8 Construction of annotations} *)

(** The signature of a monad can not be extracted from the monad
    itself, so when providing a monad for creating a type combinator
    we must also provide an expression describing the signature of the
    monad. Types will ensure that the announced signature is the same
    as the signature of the provided monad. *)

val dbyte : (_, dbyte) sannot
val dboolean : (_, dboolean) sannot
val dint16 : (_, dint16) sannot
val dint32 : (_, dint32) sannot
val dint64 : (_, dint64) sannot
val duint16 : (_, duint16) sannot
val duint32 : (_, duint32) sannot
val duint64 : (_, duint64) sannot
val ddouble : (_, ddouble) sannot
val dstring : (_, dstring) sannot
val dsignature : (_, dsignature) sannot
val dobject_path : (_, dobject_path) sannot
val dstruct : (unit, 'a) annot -> (_, 'a dstruct) sannot
val darray : (unit, 'a) sannot -> (_, 'a darray) sannot
val ddict : (unit, 'a) sannot -> (unit, 'b) sannot -> (_, ('a, 'b) ddict) sannot
val dvariant : (_, dvariant) sannot

val dpair : ('a, 'b) annot -> ('b, 'c) annot -> ('a, 'c) annot
val (@@) : ('a, 'b) annot -> ('b, 'c) annot -> ('a, 'c) annot
val dnil : ('a, 'a) annot

(** {8 Extraction of types from annotations} *)

val basic_type_of_annot : (unit, [< dbasic ]) sannot -> basic
val single_type_of_annot : (unit, 'a) sannot -> single
val sequence_type_of_annot : (unit, 'a) annot -> sequence

(** {6 Combinators (for the syntax extension)} *)

val ob_signature : (signature, _, dsignature) OBus_comb.one
