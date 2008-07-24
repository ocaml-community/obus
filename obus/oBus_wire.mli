(*
 * wire.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Serialization/deserialization *)

(** This module is used to create monads for serializing or
    deserializing DBus values.

    This can be used to create new type combinators which can not be
    written with the functions provided by the [OBus_conv] module. *)

open OBus_types

(** {6 Annotations} *)

(** In the following we will call the signature of a monad [m] the
    DBus type of the value written by [m] if it is a writing monad or
    read by [m] if it is a reading monad.

    To ensure that the signature is correct, i.e. to prevent from
    writing/reading value with an incorrect structure, we encode the
    signature in the type of the monad.

    Note that we need to encode the whole signature and not only the
    class (basic, single or sequence). This ensure that the signature
    of the monad is constant.

    For example it prevent this kind of code:

    {[
      let f x = match Random.int 2 with
        | 0 -> wint32 x
        | _ -> wuint32 x
    ]}

    This is why the types used in this module are a bit complicated...
*)

type (+'a, 'b) annot
  (** ['b] is the DBus type structure and ['a] is the tail
      variable:

      For example the sequence (INT32, STRING, BYTE ARRAY) is encoded
      like that:

      [('a, dint32 * (dstring * (dbyte darray * 'a)))]

      ['a] is used to have a canonical of sequence, for example to
      identify the following types:

      [(dint32 * dint32) * dstring] and [dint32 * dint32 * dstring]
  *)

type (+'a, 'b) sannot = ('a, 'b * 'a) annot
    (** For one type *)

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
type 'a dstruct
type 'a darray
type ('a, 'b) ddict
constraint 'a = [> ]
    (** This constraint ensure that the type is a basic type *)
type dvariant

(** {8 Construction of annotations} *)

(** The signature of a monad can not be extracted from the monad
    itself, so when providing a monad for creating a type combinator
    we must also provide an expression describing the signature of the
    monad. Types will ensure that the announced signature is the same
    as the signature of the provided monad. *)

val dbyte : (_, dbasic) sannot
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
val ddict : (unit, [> ] as 'a) sannot -> (unit, 'b) sannot -> (_, ('a, 'b) ddict) sannot
val dvariant : (_, dvariant) sannot

val dpair : ('a, 'b) annot -> ('b, 'c) annot -> ('a, 'c) annot
val dnil : ('a, 'a) annot

val dtup3 : ('a, 'b) annot -> ('b, 'c) annot -> ('c, 'd) annot -> ('a, 'd) annot
val dtup4 : ('a, 'b) annot -> ('b, 'c) annot -> ('c, 'd) annot -> ('d, 'e) annot -> ('a, 'e) annot
val dtup5 : ('a, 'b) annot -> ('b, 'c) annot -> ('c, 'd) annot -> ('d, 'e) annot -> ('e, 'f) annot -> ('a, 'f) annot

(** {8 Extraction of types from annotations} *)

val basic_type_of_annot : (unit, [> ]) sannot -> basic
val single_type_of_annot : (unit, 'a) sannot -> single
val sequence_type_of_annot : (unit, 'a) annot -> sequence

(** {6 Wire Monad} *)

(** Type of monads used for reading and writing *)
module type Wire_monad = sig
  type ('a, +'b, 'c) t
    (** ['a] is the type parameter of the monad, ['b] and ['c] have
        the same role as in [('b, 'c) annot]. *)

  type ('a, +'b, 'c) one = ('a, 'b, 'c * 'b) t
      (** A monad which read/write only ony value *)

  type ('a, +'b) null = ('a, 'b, 'b) t
      (** Monad which has no effect (read/write nothing). *)

  val bind : ('a, 'b, 'c) t -> ('a -> ('d, 'c, 't) t) -> ('d, 'b, 'e) t
  val return : 'a -> ('a, 'b) null

  val (>>=) : ('a, 'b, 'c) t -> ('a -> ('d, 'c, 'e) t) -> ('d, 'b, 'e) t
  val (>>) : (unit, 'a, 'b) t -> ('c, 'b, 'd) t -> ('c, 'a, 'd) t
end

(** {6 Writing} *)

module Writer : Wire_monad

(** {8 Basic types} *)

(** Some of these functions have aliases for convenience *)

val wbyte : char -> (unit, _, dbyte) Writer.one
val wchar : char -> (unit, _, dbyte) Writer.one
val wint8 : int -> (unit, _, dbyte) Writer.one
val wuint8 : int -> (unit, _, dbyte) Writer.one
val wint16 : int -> (unit, _, dint16) Writer.one
val wuint16 : int -> (unit, _, duint16) Writer.one
val wint : int -> (unit, _, dint32) Writer.one
val wuint : int -> (unit, _, duint32) Writer.one
val wint32 : int32 -> (unit, _, dint32) Writer.one
val wuint32 : int32 -> (unit, _, duint32) Writer.one
val wint64 : int64 -> (unit, _, dint64) Writer.one
val wuint64 : int64 -> (unit, _, duint64) Writer.one
val wdouble : float -> (unit, _, ddouble) Writer.one
val wfloat : float -> (unit, _, ddouble) Writer.one
val wboolean : bool -> (unit, _, dboolean) Writer.one
val wbool : bool -> (unit, _, dboolean) Writer.one
val wstring : string -> (unit, _, dstring) Writer.one
val wsignature : signature -> (unit, _, dsignature) Writer.one
val wobject_path : string -> (unit, _, dobject_path) Writer.one
val wpath : string -> (unit, _, dobject_path) Writer.one

(** {8 Containers} *)

val wstruct : (unit, unit, 'a) Writer.t -> (unit, _, 'a dstruct) Writer.one

type accu

val warray : (unit, 'a) sannot -> ('b -> (unit, unit, 'a) Writer.one) -> (('b -> accu -> accu) -> 'c -> accu -> accu) -> 'c -> (unit, _, 'a darray) Writer.one
  (** [warray typ element_writer fold] construct an array writer.
      [fold] must be a fold-like function for values of type ['c]

      For an obscure reason in the DBus wire protocol we need to know
      the type of the elements. *)

val wdict : ('a -> 'b -> (unit, unit, 'c * ('d * unit)) Writer.one) -> (('b -> accu -> accu) -> 'c -> accu -> accu) -> 'c -> (unit, _, 'a darray) Writer.one
  (** Same thing but for dictionnaries, the types of element is not
      required here. *)

(** {8 Predefined array writers} *)

val wlist : (unit, 'a) sannot -> ('b -> (unit, unit, 'a) Writer.one) -> 'b list -> (unit, _, 'a darray) Writer.one
  (** Write a list as an array *)

val wassoc : ('a -> 'b -> (unit, unit, 'c * ('d * unit)) Writer.t) -> ('a * 'b) list -> (unit, _, ('c, 'd) ddict) Writer.one
  (** Write an associative list as a dictionnary *)

val wbyte_array : string -> (unit, _, dbyte darray) Writer.one
  (** Write a string as an array of byte. This writer is more
      efficient the one we can write with [warray] *)

(** {8 Writing of variants} *)

(** A variant is serialized as a type followed by a value of this
    type. There is two way to write variants: *)

val wvariant : OBus_value.single -> (unit, _, dvariant) Writer.one
  (** This writer will write a variant from a dynamically typed
      value *)

val wfixed : (unit, unit, 'a) Writer.one -> (unit, _, dvariant) Writer.one
  (** This writer will write a variant from a value of a fixed
      type. For example here is a way to serialize a caml variant:

      type toto =
        | A of int (* key = 1 *)
        | B of string (* key = 2 *)
        | C of int * string (* key = 3 *)

      let toto_writer = function
        | A i -> wbyte 1 >> wfixed (wint u)
        | B s -> wbyte 2 >> wfixed (wstring s)
        | C(i, s) -> wbyte 3 >> wfixed (wstruct (wint i >> wstring s))
  *)

(** {8 Context} *)

val wconnection : (OBus_connection.t, _) Writer.null
  (** Return the connection which will be used to send the message
      being written *)

val wdestination : (OBus_bus.name option, _) Writer.null
  (** Return the destination of the message being written *)

(** {6 Reading} *)

module Reader : Wire_monad

(** {8 Basic types} *)

val rbyte : (char, _, dbyte) Reader.one
val rchar : (char, _, dbyte) Reader.one
val rint8 : (int, _, dbyte) Reader.one
val ruint8 : (int, _, dbyte) Reader.one
val rint16 : (int, _, dint16) Reader.one
val ruint16 : (int, _, duint16) Reader.one
val rint : (int, _, dint32) Reader.one
val ruint : (int, _, duint32) Reader.one
val rint32 : (int32, _, dint32) Reader.one
val ruint32 : (int32, _, duint32) Reader.one
val rint64 : (int64, _, dint64) Reader.one
val ruint64 : (int64, _, duint64) Reader.one
val rdouble : (float, _, ddouble) Reader.one
val rfloat : (float, _, ddouble) Reader.one
val rboolean : (bool, _, dboolean) Reader.one
val rbool : (bool, _, dboolean) Reader.one
val rstring : (string, _, dstring) Reader.one
val rsignature : (signature, _, dsignature) Reader.one
val robject_path : (string, _, dobject_path) Reader.one
val rpath : (string, _, dobject_path) Reader.one

(** {8 Containers} *)

val rstruct : ('a, unit, 'b) Reader.t -> ('a, _, 'b dstruct) Reader.one

val rarray : ?reverse:bool -> (unit, 'a) sannot -> ('b, unit, 'a) Reader.one -> ('b -> 'c -> 'c) -> 'c -> ('c, _, 'a darray) Reader.one
  (** [rarray reverse typ element_reader add empty] construct an array reader.

      [add] is a function which add an element to a value of type ['c] and
      [empty] is the initial value of the acumulator.

      Note that by default the element are added using [add] in the order
      they are marshaled, so for example this code will construct a list in
      reverse order:

      [rarray duint32 ruint (fun x l -> x :: l) []]

      Passing [true] for [reverse] will change this behaviour but the
      function will not be tail-recursive.  *)

val rdict : ?reverse:bool -> ('a * 'b, unit, 'c * ('d * unit)) Reader.t -> ('a -> 'b -> 'e -> 'e) -> 'e -> ('e, _, ('c, 'd) ddict) Reader.one
  (** Same thing but for dictionnaries *)

(** {8 Predefined array readers} *)

val rlist : (unit, 'a) sannot -> ('b, unit, 'a) Reader.one -> ('b list, _, 'a darray) Reader.one
  (** Read a list. The order of element is kept with this reader *)

val rset : (unit, 'a) sannot -> ('b, unit, 'a) Reader.one -> ('b list, _, 'a darray) Reader.one
  (** Same thing as [rlist] but the list will be in reverse order. *)

val rassoc : ('a * 'b, unit, 'c * ('d * unit)) Reader.t -> (('a * 'b) list, _, ('c, 'd) ddict) Reader.one
  (** Read a dictionnary as a an associative list. Elements are in
      reverse order. *)

val rbyte_array : (string, _, dbyte darray) Reader.one
  (** Read an array of byte as a string *)

(** {8 Reading of variants} *)

val rvariant : (OBus_value.one, _, dvariant) Reader.one
  (** Read a variant as a dynamic value *)

val rfixed : ('a, unit, 'b) Reader.one -> ('a, _, dvariant) Reader.one
  (** Read a variant with a fixed reader. It will fail if types do not
      match. *)

(** {8 Context} *)

val rconnection : (OBus_connection.t, _) Reader.null
  (** Return the current from which came the message *)

val rsender : (OBus_bus.name option, _) Reader.null
  (** Return the sender of the message *)
