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

(** {6 Annotations} *)

(** The following types are used as annotations of reader/writer
    monads and type combinators to ensure at compile-time that the
    structure of values manipulated respect the DBus format.

    For example the following constructions will be rejected because
    they are incorrect:

    [ob_dict (ob_array ob_int32) ob_string] (because the key type of a
    dictionary must be a basic type)

    [ob_array (ob_pair ob_int32 ob_string)] (because type of element
    of an array must consist of one single type)

    Moreover, in case of reader/writer monads, annotations are used to
    ensure that the signature provided with the monad is really the
    type of value read/written by the monad.
*)

type (+'a, +'b) annot
  (** ['b] contains the structure of the type and ['a] is the tail
      variable, which is used to have canonical representation of
      sequences.

      For example the sequence [tint32, tstring, tbyte tarray] is
      encoded like that:

      [('a, tint32 * (tstring * (tbyte tarray * 'a))) annot] *)

type (+'a, +'b) one = ('a, 'b * 'a) annot
    (** Annotation for one single type *)

type abasic =
  [ `byte
  | `boolean
  | `int16
  | `int32
  | `int64
  | `uint16
  | `uint32
  | `uint64
  | `double
  | `string
  | `signature
  | `object_path ]

type 'a basic_p = (unit, 'a) one
constraint 'a = [< abasic ]
    (** Match annotations of one basic type *)
type 'a single_p = (unit, 'a) one
    (** Match annotations of one single type *)
type 'a sequence_p = (unit, 'a) annot
    (** Match any annotations *)

(** {8 Annotations -> types} *)

val basic_of_annot : 'a basic_p -> basic
val single_of_annot : 'a single_p -> single
val sequence_of_annot : 'a sequence_p -> sequence

(** {8 Typed constructors} *)

val dbyte : (_, [`byte]) one
val dboolean : (_, [`boolean]) one
val dint16 : (_, [`int16]) one
val dint32 : (_, [`int32]) one
val dint64 : (_, [`int64]) one
val duint16 : (_, [`uint16]) one
val duint32 : (_, [`uint32]) one
val duint64 : (_, [`uint64]) one
val ddouble : (_, [`double]) one
val dstring : (_, [`string]) one
val dsignature : (_, [`signature]) one
val dobject_path : (_, [`object_path]) one

val dstruct : 'a sequence_p -> (_, [`structure of 'a]) one
val darray : 'a single_p -> (_, [`array of 'a]) one
val ddict : 'a basic_p -> 'b single_p -> (_, [`dict of 'a * 'b]) one
val dvariant : (_, [`variant]) one

val dpair : ('a, 'b) annot -> ('c, 'a) annot -> ('c, 'b) annot
val (++) : ('a, 'b) annot -> ('c, 'a) annot -> ('c, 'b) annot
val dnil : ('a, 'a) annot
