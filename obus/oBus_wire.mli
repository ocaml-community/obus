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

open OBus_types

(** {6 Wire Monad} *)

type reader
type writer

type ('a, +'b, +'c, 'typ) t = ('a, 'b, 'c, 'typ) OBus_intern.wire
  (** Type of a wire monad. ['a] is the type parameter of the monad,
      ['b] and ['c] have the same role as in [('b, 'c) annot]. ['typ]
      is one of [reader] or [writer]. *)

type ('a, +'b, +'c, 'typ) one = ('a, 'b, 'c * 'b, 'typ) t
    (** A monad which read/write only one single value *)

type ('a, +'b, 'typ) null = ('a, 'b, 'b, 'typ) t
    (** Monad which has no effect (read/write nothing). *)

type ('a, 'b, 'typ) basic_p = ('a, unit, 'b, 'typ) one
constraint 'b = _ dbasic
    (** Match monads which have a signature of one basic type *)
type ('a, 'b, 'typ) single_p = ('a, unit, 'b, 'typ) one
    (** Match monads which have a signature of one single type *)
type ('a, 'b, 'typ) sequence_p = ('a, unit, 'b, 'typ) t
    (** Match any monads *)

val bind : ('a, 'b, 'c, 'typ) t -> ('a -> ('d, 'e, 'b, 'typ) t) -> ('d, 'e, 'c, 'typ) t
val return : 'a -> ('a, 'b, 'typ) null

val failwith : string -> ('a, 'b, 'c, 'typ) t

val (>>=) : ('a, 'b, 'c, 'typ) t -> ('a -> ('d, 'e, 'b, 'typ) t) -> ('d, 'e, 'c, 'typ) t
val (>>) : (_, 'a, 'b, 'typ) t -> ('c, 'd, 'a, 'typ) t -> ('c, 'd, 'b, 'typ) t

val run : ('a, 'b, 'c, 'typ) t -> OBus_intern.connection -> string option -> OBus_info.byte_order -> string -> int -> int * 'a
  (** [run monad connection sender/destination byte_order buffer ptr]
      Run a monad on the given buffer. It return the buffer position
      after the execution of the monad and the result. *)

(** {6 Writing} *)

(** {8 Basic types} *)

(** Some of these functions have aliases for convenience *)

val wbyte : char -> (unit, _, dbyte, writer) one
val wchar : char -> (unit, _, dbyte, writer) one
val wint8 : int -> (unit, _, dbyte, writer) one
val wuint8 : int -> (unit, _, dbyte, writer) one
val wint16 : int -> (unit, _, dint16, writer) one
val wuint16 : int -> (unit, _, duint16, writer) one
val wint : int -> (unit, _, dint32, writer) one
val wuint : int -> (unit, _, duint32, writer) one
val wint32 : int32 -> (unit, _, dint32, writer) one
val wuint32 : int32 -> (unit, _, duint32, writer) one
val wint64 : int64 -> (unit, _, dint64, writer) one
val wuint64 : int64 -> (unit, _, duint64, writer) one
val wdouble : float -> (unit, _, ddouble, writer) one
val wfloat : float -> (unit, _, ddouble, writer) one
val wboolean : bool -> (unit, _, dboolean, writer) one
val wbool : bool -> (unit, _, dboolean, writer) one
val wstring : string -> (unit, _, dstring, writer) one
val wsignature : signature -> (unit, _, dsignature, writer) one
val wobject_path : string -> (unit, _, dobject_path, writer) one
val wpath : string -> (unit, _, dobject_path, writer) one

(** {8 Containers} *)

val wstruct : (unit, 'da, writer) sequence_p -> (unit, _, 'da dstruct, writer) one

type accu

val warray : 'da OBus_types.single_p -> ('a -> (unit, 'da, writer) single_p) ->
  (('a -> accu -> accu) -> accu -> accu) -> (unit, _, 'da darray, writer) one
    (** [warray typ element_writer fold] construct an array writer.
        [fold] must be a fold-like function for values of type ['c]

        Due to an obscure reason in the DBus wire protocol we need to
        know the type of the elements. *)

val wdict : ('a -> (unit, 'da * ('db * unit), writer) sequence_p) ->
  (('a -> accu -> accu) -> accu -> accu) -> (unit, _, ('da, 'db) ddict, writer) one
    (** Same thing but for dictionnaries, the type of elements is not
        required here. *)

(** It also possible to make array writers by using the following
    monoid: *)
module Seq : sig
  type 'a t
    (** ['a] is the DBus annotation *)

  val empty : 'a t
  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t list -> 'a t

  val one : (unit, 'a, writer) sequence_p -> 'a t
    (** Make an element from a writer *)

(** Here is an example of array writer which can be constructed with
    this monoid:

    Suppose you have a record of booleans and you want to represent it
    as a set of integers corresponding to the field which are true:

    {[
      type flags = {
        flag_a : bool; (* = 1 *)
        flag_b : bool; (* = 2 *)
        flag_c : bool; (* = 3 *)
      }

      let flag_writer x = function
        | true -> Seq.one (wuint x)
        | false -> Seq.empty

      let (>>>) = List_writer.append

      let flags_writer flags =
        warray_seq duint32
          (flag_writer 1 flags.flag_a
           >>> flag_writer 2 flags.flag_b
           >>> flag_writer 3 flags.flag_c)
    ]}
*)

(** Note that it is possible to construct all array writer with this
    monoid but it is less efficient than with [warray] and [wdict] *)
end

val warray_seq : 'da OBus_types.single_p -> ('da * unit) Seq.t -> (unit, _, 'da darray, writer) one
val wdict_seq : ('k * ('v * unit)) Seq.t -> (unit, _, ('k, 'v) ddict, writer) one

(** {8 Predefined array writers} *)

val wlist : 'da OBus_types.single_p -> ('a -> (unit, 'da, writer) single_p) -> 'a list -> (unit, _, 'da darray, writer) one
  (** Write a list as an array *)

val wassoc : ('a -> (unit, 'da, writer) basic_p) -> ('b -> (unit, 'db, writer) single_p) ->
  ('a * 'b) list -> (unit, _, ('da, 'db) ddict, writer) one
  (** Write an associative list as a dictionnary *)

val wbyte_array : string -> (unit, _, dbyte darray, writer) one
  (** Write a string as an array of byte. This writer is more
      efficient the one we can write with [warray] *)

(** {8 Writing of variants} *)

(** A variant is serialized as a type followed by a value of this
    type. There is two way to write variants: *)

val wvariant : OBus_value.single -> (unit, _, dvariant, writer) one
  (** This writer will write a variant from a dynamically typed
      value *)

val wfixed : 'da OBus_types.single_p -> (unit, 'da, writer) single_p -> (unit, _, dvariant, writer) one
  (** This writer will write a variant from a value of a fixed
      type. For example here is a way to serialize a caml variant:

      type toto =
        | A of int (* key = 1 *)
        | B of string (* key = 2 *)
        | C of int * string (* key = 3 *)

      let toto_writer = function
        | A i -> wbyte 1 >> wfixed dint32 (wint u)
        | B s -> wbyte 2 >> wfixed dstring (wstring s)
        | C(i, s) -> wbyte 3 >> wfixed (wstruct (wint i >> wstring s))
  *)

(** {8 Context} *)

val wconnection : (OBus_intern.connection, _, writer) null
  (** Return the connection which will be used to send the message
      being written *)

val wdestination : (string option, _, writer) null
  (** Return the destination of the message being written *)

(** {6 Reading} *)

(** {8 Basic types} *)

val rbyte : (char, _, dbyte, reader) one
val rchar : (char, _, dbyte, reader) one
val rint8 : (int, _, dbyte, reader) one
val ruint8 : (int, _, dbyte, reader) one
val rint16 : (int, _, dint16, reader) one
val ruint16 : (int, _, duint16, reader) one
val rint : (int, _, dint32, reader) one
val ruint : (int, _, duint32, reader) one
val rint32 : (int32, _, dint32, reader) one
val ruint32 : (int32, _, duint32, reader) one
val rint64 : (int64, _, dint64, reader) one
val ruint64 : (int64, _, duint64, reader) one
val rdouble : (float, _, ddouble, reader) one
val rfloat : (float, _, ddouble, reader) one
val rboolean : (bool, _, dboolean, reader) one
val rbool : (bool, _, dboolean, reader) one
val rstring : (string, _, dstring, reader) one
val rsignature : (signature, _, dsignature, reader) one
val robject_path : (string, _, dobject_path, reader) one
val rpath : (string, _, dobject_path, reader) one

(** {8 Containers} *)

val rstruct : ('a, 'da, reader) sequence_p -> ('a, _, 'da dstruct, reader) one

val rarray : 'da OBus_types.single_p -> ('a -> ('a, 'da, reader) single_p) -> 'a -> ('a, _, 'da darray, reader) one
  (** [rarray typ reader acc] construct an array reader.

      [reader] must read an element and add it to the accumulator, and
      [acc] is the initial value of the accumulator. *)

val rdict : ('a -> ('a, 'k * ('v * unit), reader) sequence_p) -> 'a -> ('a, _, ('k, 'v) ddict, reader) one
  (** Same thing but for dictionnaries *)

(** {8 Predefined array readers} *)

val rlist : 'da OBus_types.single_p -> ('a, 'da, reader) single_p -> ('a list, _, 'da darray, reader) one
  (** Read a list. The order of element is kept with this reader *)

val rset : 'da OBus_types.single_p -> ('a, 'da, reader) single_p -> ('a list, _, 'da darray, reader) one
  (** Same thing as [rlist] but the list will be in reverse order. *)

val rassoc : ('a, 'da, reader) basic_p -> ('b, 'db, reader) single_p ->
  (('a * 'b) list, _, ('da, 'db) ddict, reader) one
    (** Read a dictionnary as a an associative list. Elements are in
        reverse order. *)

val rbyte_array : (string, _, dbyte darray, reader) one
  (** Read an array of byte as a string *)

(** {8 Reading of variants} *)

val rvariant : (OBus_value.single, _, dvariant, reader) one
  (** Read a variant as a dynamic value *)

val rfixed : 'da OBus_types.single_p -> ('a, 'da, reader) single_p -> ('a, _, dvariant, reader) one
  (** Read a variant with a fixed reader. It will fail if types do not
      match. *)

(** {8 Context} *)

val rconnection : (OBus_intern.connection, _, reader) null
  (** Return the current from which came the message *)

val rsender : (string option, _, reader) null
  (** Return the sender of the message *)

(**/**)

type priv
val wsequence : OBus_value.sequence -> (unit, priv, priv, writer) t
val rsequence : OBus_types.sequence -> (OBus_value.sequence, priv, priv, reader) t
