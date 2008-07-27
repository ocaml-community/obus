(*
 * oBus_annot.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Annotations *)

(** There is two purpose of annotations:

    - when defining a service the type we give to methods, signals and
    properties contains annotated DBus types which will be reported in
    the introspection so OBus can use it while producing a binding
    skeleton.

    - specifying the effect of a writing/reading monad so the type
    checker can ensure that it produce/read correct DBus value and it
    match the announced effect.
*)

(** {6 Extended types} *)

type ext_basic =
  | Ta_byte
  | Ta_boolean
  | Ta_int8
  | Ta_uint8
  | Ta_int16
  | Ta_int32
  | Ta_int64
  | Ta_uint16
  | Ta_uint32
  | Ta_uint64
  | Ta_int
  | Ta_uint
  | Ta_double
  | Ta_string
  | Ta_signature
  | Ta_object_path
  | Ta_flag of ext_basic * string * string list
  | Ta_bitwise of ext_basic * string * string list

type ext_single =
  | Ta_basic of ext_basic
  | Ta_struct of ext_sequence
  | Ta_array of ext_single
  | Ta_dict of ext_basic * ext_single
  | Ta_byte_array
  | Ta_variant

and ext_sequence =
  | Ta_cons of ext_sequence * ext_sequence
  | Ta_one of ext_single
  | Ta_nil
      (** This representation is adapted to the construction *)

val map_ext_sequence : (ext_single -> 'a) -> ext_sequence -> 'a list
  (** Map a sequence of annotation into a list *)

(** {6 Type of annotations} *)

type (+'a, +'b) t = private Annot of ext_sequence
    (** ['b] is the DBus type structure and ['a] is the tail
        variable:

        For example the sequence (INT32, STRING, BYTE ARRAY) is encoded
        like that:

        [('a, dint32 * (dstring * (dbyte darray * 'a))) annot]

        ['a] is used to have a canonical representation of sequence, for
        example to identify the following types:

        [(dint32 * dint32) * dstring] and [dint32 * dint32 * dstring]
    *)

type (+'a, +'b) one = ('a, 'b * 'a) t
    (** Annotation for a single type *)

(** {8 Basic types} *)

type dbasic_byte
type dbasic_boolean
type dbasic_int8
type dbasic_uint8
    (** [int8] and [uint8] corresponds to a DBus byte seen as an
        signed/unsigned integer *)
type dbasic_int16
type dbasic_int32
type dbasic_int64
type dbasic_uint16
type dbasic_uint32
type dbasic_uint64
type dbasic_int
type dbasic_uint
    (** [int] and [uint] corresponds to DBus int32 and uint32 casted
        into a native caml integer *)
type dbasic_double
type dbasic_string
type dbasic_signature
type dbasic_object_path

(** {8 Basic types seen as single types} *)

type 'a dbasic

type dbyte = dbasic_byte dbasic
type dboolean = dbasic_boolean dbasic
type dint8 = dbasic_int8 dbasic
type duint8 = dbasic_uint8 dbasic
type dint16 = dbasic_int16 dbasic
type dint32 = dbasic_int32 dbasic
type dint64 = dbasic_int64 dbasic
type duint16 = dbasic_uint16 dbasic
type duint32 = dbasic_uint32 dbasic
type duint64 = dbasic_uint64 dbasic
type dint = dbasic_int dbasic
type duint = dbasic_uint dbasic
type ddouble = dbasic_double dbasic
type dstring = dbasic_string dbasic
type dsignature = dbasic_signature dbasic
type dobject_path = dbasic_object_path dbasic

(** {8 Container types} *)

type 'a dstruct
type 'a darray
type dbyte_array
type ('a, 'b) ddict
constraint 'a = _ dbasic
type dvariant

(** {6 Construction of typed annotations} *)

(** The signature of a monad can not be extracted from the monad
    itself, so when providing a monad for creating a type combinator
    we must also provide an expression describing the signature of the
    monad. Types will ensure that the announced signature is the same
    as the signature of the provided monad. *)

type 'a basic_p = (unit, 'a) one
constraint 'a = _ dbasic
    (** Match annotations of one basic type *)
type 'a single_p = (unit, 'a) one
    (** Match annotations of one single type *)
type 'a sequence_p = (unit, 'a) t
    (** Match any annotations *)

val dbyte : (_, dbyte) one
val dint8 : (_, dint8) one
val duint8 : (_, duint8) one
val dboolean : (_, dboolean) one
val dint16 : (_, dint16) one
val dint32 : (_, dint32) one
val dint64 : (_, dint64) one
val duint16 : (_, duint16) one
val duint32 : (_, duint32) one
val duint64 : (_, duint64) one
val dint : (_, dint) one
val duint : (_, duint) one
val ddouble : (_, ddouble) one
val dstring : (_, dstring) one
val dsignature : (_, dsignature) one
val dobject_path : (_, dobject_path) one

val dflag : 'a single_p -> string -> string list -> (_, 'a) one
val dbitwise : 'a sequence_p -> string -> string list -> (_, 'a) one

val dstruct : 'a sequence_p -> (_, 'a dstruct) one
val darray : 'a single_p -> (_, 'a darray) one
val dbyte_array : (_, dbyte_array) one
val ddict : 'a basic_p -> 'b single_p -> (_, ('a, 'b) ddict) one
val dvariant : (_, dvariant) one

val dpair : ('a, 'b) t -> ('c, 'a) t -> ('c, 'b) t
val (++) : ('a, 'b) t -> ('c, 'a) t -> ('c, 'b) t
val dnil : ('a, 'a) t

(** {6 Convertion} *)

val ext_basic_of_annot : 'a basic_p -> ext_basic
val ext_single_of_annot : 'a single_p -> ext_single
val ext_sequence_of_annot : 'a sequence_p -> ext_sequence
  (** Extract the untyped structure from an annotation. *)

val basic_type_of_ext : ext_basic -> OBus_types.basic
val single_type_of_ext : ext_single -> OBus_types.single
val sequence_type_of_ext : ext_sequence -> OBus_types.sequence
  (** Return the standart DBus type of an extended type *)

val default_ext_of_basic : OBus_types.basic -> ext_basic
val default_ext_of_single : OBus_types.single -> ext_single
val default_ext_of_sequence : OBus_types.sequence -> ext_sequence
  (** Return the default extended type of a standart DBus type *)

val canonicalize_basic : ext_basic -> ext_basic
val canonicalize_single : ext_single -> ext_single
val canonicalize_sequence : ext_sequence -> ext_sequence
  (** Canonicalize an extended type *)

(** {6 Printing} *)

val string_of_basic : bool -> ext_basic -> string
val string_of_single : bool -> ext_single -> string
val string_of_sequence : bool -> ext_sequence -> string
  (** Return a caml-type style representation of a extended type. If
      the boolean flag is [true] the string represent an obus
      combinator, if [false] it is just a standard caml type *)

(**/**)

type dunknown
val make_unknown : ('a, 'b) t -> (dunknown, dunknown) t
