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

open OBus_annot
open OBus_wire

(** {6 Simple combinators} *)

type ('a, +'b, +'c) t = {
  annot : ('b, 'c) OBus_annot.t;
  reader : ('a, 'b, 'c, reader) OBus_wire.t;
  writer : 'a -> (unit, 'b, 'c, writer) OBus_wire.t;
}

type ('a, +'b, +'c) one = ('a, 'b, 'c * 'b) t
    (** Sequence of length 1 *)

type ('a, 'b) basic_p = ('a, unit, 'b) one
constraint 'b = _ dbasic
    (** Match combinators with a signature of one basic type *)
type ('a, 'b) single_p = ('a, unit, 'b) one
    (** Match combinators with a signature of length 1 *)
type ('a, 'b) sequence_p = ('a, unit, 'b) t
    (** Match all combinators *)

val annot : ('a, 'b, 'c) t -> ('b, 'c) OBus_annot.t
  (** Return the annotation of a combinator *)

val reader : ('a, 'b, 'c) t -> ('a, 'b, 'c, reader) OBus_wire.t
  (** Return the reader monad of a type combinator *)

val writer : ('a, 'b, 'c) t -> 'a -> (unit, 'b, 'c, writer) OBus_wire.t
  (** Return the writer monad of a type combinator *)

val make :
  annot:('b, 'c) OBus_annot.t ->
  reader:('a, 'b, 'c, reader) OBus_wire.t ->
  writer:('a -> (unit, 'b, 'c, writer) OBus_wire.t) ->
  ('a, 'b, 'c) t

val wrap : ('a, 'b, 'c) t -> ('a -> 'd) -> ('d -> 'a) -> ('d, 'b, 'c) t
  (** Wrap a convertor with the given functions *)

(** {6 Functionnal combinators} *)

type ('a, 'b, 'c) func
  (** Type of a functionnal combinator, ['b] is a variable which will
      be instancied with the return type of the function, ['a] is the
      function type and ['c] is the real return type of the
      function *)

val reply : ('a, 'b) sequence_p -> ('c, 'c, 'a) func
  (** Create a functionnal combinator from a simple one *)

val abstract : ('a, 'da) sequence_p -> ('b, 'c, 'd) func -> ('a -> 'b, 'c, 'd) func
  (** Create a functionnal combinator *)

(**/**)

val func_signature : ('a, 'b, 'c) func -> ext_sequence
val func_reply : ('a, 'b, 'c) func -> ('c, dunknown, dunknown) t
val func_send : ('a, 'b, 'c) func -> ((unit, dunknown, dunknown, writer) OBus_wire.t -> 'b) -> 'a
val func_recv : ('a, 'b, 'c) func -> ('a -> 'b, dunknown, dunknown, reader) OBus_wire.t
