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
    written with the functions provided by the [OBus_conv] module.

    The type of monads contains their signatures (= the type of the
    DBus values they write or read) so monad combination is ensured to
    be safe (in the sens that a monad which will pass the type checker
    will always produce or read correct DBus values).

    So it is safe to write your own writing/reading monad.
*)

open OBus_annot

(** {6 Wire Monad} *)

(** Type of monads used for reading and writing *)
module type Wire_monad = sig
  type ('a, +'b, +'c) t
    (** ['a] is the type parameter of the monad, ['b] and ['c] have
        the same role as in [('b, 'c) annot]. *)

  type ('a, +'b, +'c) one = ('a, 'b, 'c * 'b) t
      (** A monad which read/write only one single value *)

  type ('a, +'b) null = ('a, 'b, 'b) t
      (** Monad which has no effect (read/write nothing). *)

  type ('a, 'b) basic_p = ('a, unit, 'b) one
  constraint 'b = _ dbasic
      (** Match monads which have a signature of one basic type *)
  type ('a, 'b) single_p = ('a, unit, 'b) one
      (** Match monads which have a signature of one single type *)
  type ('a, 'b) sequence_p = ('a, unit, 'b) t
      (** Match any monads *)

  val bind : ('a, 'b, 'c) t -> ('a -> ('d, 'c, 't) t) -> ('d, 'b, 'e) t
  val return : 'a -> ('a, 'b) null

  val failwith : string -> ('a, 'b, 'c) t

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

val wstruct : (unit, 'da) Writer.sequence_p -> (unit, _, 'da dstruct) Writer.one

type accu

val warray : 'da OBus_annot.single_p -> ('a -> (unit, 'da) Writer.single_p) ->
  (('a -> accu -> accu) -> 'b -> accu -> accu) -> 'b ->
  (unit, _, 'da darray) Writer.one
    (** [warray typ element_writer fold] construct an array writer.
        [fold] must be a fold-like function for values of type ['c]

        Due to an obscure reason in the DBus wire protocol we need to
        know the type of the elements. *)

val wdict :
  ('a -> (unit, 'da) Writer.basic_p) ->
  ('b -> (unit, 'db) Writer.single_p) ->
  (('a -> 'b -> accu -> accu) -> 'c -> accu -> accu) -> 'c ->
  (unit, _, ('da, 'db) ddict) Writer.one
    (** Same thing but for dictionnaries, the type of elements is not
        required here. *)

(** {8 Predefined array writers} *)

val wlist : 'da OBus_annot.single_p -> ('a -> (unit, 'da) Writer.single_p) -> 'b list -> (unit, _, 'a darray) Writer.one
  (** Write a list as an array *)

val wassoc : ('a -> (unit, 'da) Writer.basic_p) -> ('b -> (unit, 'db) Writer.single_p) ->
  ('a * 'b) list -> (unit, _, ('da, 'db) ddict) Writer.one
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

val wfixed : (unit, 'da) Writer.single_p -> (unit, _, dvariant) Writer.one
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

val rstruct : ('a, 'da) Reader.sequence_p -> ('a, _, 'da dstruct) Reader.one

val rarray : ?reverse:bool -> 'da OBus_annot.single_p ->
  ('a, 'da) Reader.single_p ->
  ('a -> 'b -> 'b) -> 'b -> ('b, _, 'da darray) Reader.one
  (** [rarray reverse typ element_reader add empty] construct an array
      reader.

      [add] is a function which add an element to a value of type ['c] and
      [empty] is the initial value of the acumulator.

      Note that by default the element are added using [add] in the order
      they are marshaled, so for example this code will construct a list in
      reverse order:

      [rarray duint32 ruint (fun x l -> x :: l) []]

      Passing [true] for [reverse] will change this behaviour but the
      function will not be tail-recursive.  *)

val rdict : ?reverse:bool -> ('a, 'da) Reader.basic_p -> ('b, 'db) Reader.single_p ->
  ('a -> 'b -> 'c -> 'c) -> 'c -> ('c, _, ('da, 'da) ddict) Reader.one
  (** Same thing but for dictionnaries *)

(** {8 Predefined array readers} *)

val rlist : 'da OBus_annot.single_p -> ('a, 'da) Reader.single_p -> ('a list, _, 'da darray) Reader.one
  (** Read a list. The order of element is kept with this reader *)

val rset : 'da OBus_annot.single_p -> ('a, 'da) Reader.single_p -> ('a list, _, 'da darray) Reader.one
  (** Same thing as [rlist] but the list will be in reverse order. *)

val rassoc : ('a, 'da) Reader.basic_p -> ('b, 'db) Reader.single_p ->
  (('a * 'b) list, _, ('da, 'db) ddict) Reader.one
    (** Read a dictionnary as a an associative list. Elements are in
        reverse order. *)

val rbyte_array : (string, _, dbyte darray) Reader.one
  (** Read an array of byte as a string *)

(** {8 Reading of variants} *)

val rvariant : (OBus_value.one, _, dvariant) Reader.one
  (** Read a variant as a dynamic value *)

val rfixed : ('a, 'da) Reader.single_p -> ('a, _, dvariant) Reader.one
  (** Read a variant with a fixed reader. It will fail if types do not
      match. *)

(** {8 Context} *)

val rconnection : (OBus_connection.t, _) Reader.null
  (** Return the current from which came the message *)

val rsender : (OBus_bus.name option, _) Reader.null
  (** Return the sender of the message *)
