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

type (+'a, +'b) annot = private Annot of (sequence -> sequence)
  (** ['b] contains the structure of the type and ['a] is the tail
      variable, which is used to have canonical representation of
      sequences.

      For example the sequence [tint32, tstring, tbyte tarray] is
      encoded like that:

      [('a, tint32 * (tstring * (tbyte tarray * 'a))) annot] *)

type (+'a, +'b) one = ('a, 'b * 'a) annot
    (** Annotation for one single type *)

(** {8 Basic types} *)

type dbasic_byte
type dbasic_boolean
type dbasic_int16
type dbasic_int32
type dbasic_int64
type dbasic_uint16
type dbasic_uint32
type dbasic_uint64
type dbasic_double
type dbasic_string
type dbasic_signature
type dbasic_object_path

(** {8 Basic types seen as single types} *)

type 'a dbasic

type dbyte = dbasic_byte dbasic
type dboolean = dbasic_boolean dbasic
type dint16 = dbasic_int16 dbasic
type dint32 = dbasic_int32 dbasic
type dint64 = dbasic_int64 dbasic
type duint16 = dbasic_uint16 dbasic
type duint32 = dbasic_uint32 dbasic
type duint64 = dbasic_uint64 dbasic
type ddouble = dbasic_double dbasic
type dstring = dbasic_string dbasic
type dsignature = dbasic_signature dbasic
type dobject_path = dbasic_object_path dbasic

(** {8 Container types} *)

type 'a dstruct
type 'a darray
type ('a, 'b) ddict
constraint 'a = _ dbasic
type dvariant

(** {8 Annotations -> types} *)

type 'a basic_p = (unit, 'a) one
constraint 'a = _ dbasic
    (** Match annotations of one basic type *)
type 'a single_p = (unit, 'a) one
    (** Match annotations of one single type *)
type 'a sequence_p = (unit, 'a) annot
    (** Match any annotations *)

val basic_of_annot : 'a basic_p -> basic
val single_of_annot : 'a single_p -> single
val sequence_of_annot : 'a sequence_p -> sequence

(** {8 Typed constructors} *)

val dbyte : (_, dbyte) one
val dboolean : (_, dboolean) one
val dint16 : (_, dint16) one
val dint32 : (_, dint32) one
val dint64 : (_, dint64) one
val duint16 : (_, duint16) one
val duint32 : (_, duint32) one
val duint64 : (_, duint64) one
val ddouble : (_, ddouble) one
val dstring : (_, dstring) one
val dsignature : (_, dsignature) one
val dobject_path : (_, dobject_path) one

val dstruct : 'a sequence_p -> (_, 'a dstruct) one
val darray : 'a single_p -> (_, 'a darray) one
val ddict : 'a basic_p -> 'b single_p -> (_, ('a, 'b) ddict) one
val dvariant : (_, dvariant) one

val dpair : ('a, 'b) annot -> ('c, 'a) annot -> ('c, 'b) annot
val (++) : ('a, 'b) annot -> ('c, 'a) annot -> ('c, 'b) annot
val dnil : ('a, 'a) annot
