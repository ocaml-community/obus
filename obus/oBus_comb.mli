(*
 * oBus_comb.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Construction of methods/signals/properties types *)

(** This modules define combinators which must be used to describe the
    type of methods, signals and properties.

    The [pa_obus] syntax extension allow to write these combinators as
    caml types.

    All the predefined combinators are in [OBus_pervasives], this
    module only permit you to create new combinators.
*)

open OBus_types
open OBus_wire

(** {6 Simple combinators} *)

type ('a, +'b, +'c) t = {
  annot : ('b, 'c) OBus_types.annot;
  reader : ('a, 'b, 'c, reader) OBus_wire.t;
  writer : 'a -> (unit, 'b, 'c, writer) OBus_wire.t;
}

type ('a, +'b, +'c) one = ('a, 'b, 'c * 'b) t
    (** Sequence of length 1 *)

type ('a, 'b) basic_p = ('a, unit, 'b) one
constraint 'b = [< abasic ]
    (** Match combinators with a signature of one basic type *)
type ('a, 'b) single_p = ('a, unit, 'b) one
    (** Match combinators with a signature of length 1 *)
type ('a, 'b) sequence_p = ('a, unit, 'b) t
    (** Match all combinators *)

val annot : ('a, 'b, 'c) t -> ('b, 'c) OBus_types.annot
  (** Return the annotation of a combinator *)

val reader : ('a, 'b, 'c) t -> ('a, 'b, 'c, reader) OBus_wire.t
  (** Return the reader monad of a type combinator *)

val writer : ('a, 'b, 'c) t -> 'a -> (unit, 'b, 'c, writer) OBus_wire.t
  (** Return the writer monad of a type combinator *)

val make :
  annot:('b, 'c) OBus_types.annot ->
  reader:('a, 'b, 'c, reader) OBus_wire.t ->
  writer:('a -> (unit, 'b, 'c, writer) OBus_wire.t) ->
  ('a, 'b, 'c) t

val wrap : ('a, 'b, 'c) t -> ('a -> 'd) -> ('d -> 'a) -> ('d, 'b, 'c) t
  (** Wrap a convertor with the given functions *)

(** {6 Functionnal combinators} *)

type ('a, 'b, 'c, 'd, 'e) func
  (** Type of a functionnal combinator, ['b] is a variable which will
      be instancied with the return type of the function, ['a] is the
      function type and ['c] is the real return type of the
      function.

      [(unit, 'd) OBus_types.annot] is the input signature of the
      combinator, and [(unit, 'e) OBus_types.annot] is its output
      signature *)

val reply : ('a, 'b) sequence_p -> ('c, 'c, 'a, unit, 'b) func
  (** Create a functionnal combinator from a simple one *)

val abstract : ('a, 'db, 'da) t -> ('b, 'c, 'd, 'db, 'e) func -> ('a -> 'b, 'c, 'd, 'da, 'e) func
  (** Create a functionnal combinator *)

val func_signature : ('a, 'b, 'c, 'd, 'e) func -> (unit, 'd) OBus_types.annot
  (** Return the input signature of a functionnal combinator *)

val func_reply : ('a, 'b, 'c, 'd, 'e) func -> ('c, unit, 'e) t
  (** Return the return combinator of a functionnal combinator *)

val func_make_writer : ('a, 'b, 'c, 'd, 'e) func -> ((unit, unit, 'd, writer) OBus_wire.t -> 'b) -> 'a
  (** [func_make_writer func cont ...] make a writer from [func] and
      the remaining arguments and pass it to [cont] *)

val func_make_reader : ('a, 'b, 'c, 'd, 'e) func -> ('a -> 'b, unit, 'd, reader) OBus_wire.t
  (** [func_make_reader func] make a reader from [func] *)
