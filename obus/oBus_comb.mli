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

type ('a, +'b, +'c) simple
  (** Type of simple combinators, ['a] is the caml type of the
      combinator, ['c] the DBus type and ['b] the tail variable *)

type ('a, +'b, +'c) one = ('a, 'b, 'c * 'b) simple
    (** Sequence of length 1 *)

type (+'function_type, 'return_type, +'var) func = ('function_type, 'return_type, 'var) OBus_intern_comb.func
  (** Type of functionnal combinators. ['function_type] is the
      complete type of the method/signal, ['return_type] is the type
      of values returned by the method, or [unit] for signal and
      ['var] is used to choose the return type of the function
      according to the calling mode. *)

val annot : ('a, 'b, 'c) simple -> ('b, 'c) annot

val return : ('a, unit, 'b) simple -> ('c, 'a, 'c) func
  (** Create a functionnal combinator from a simple one *)

val abstract : ('a, unit, 'da) simple -> ('b, 'c, 'd) func -> ('a -> 'b, 'c, 'd) func
  (** Create a functionnal combinator *)

(** {6 Construction of new convertor} *)

val wrap : ('a, 'b, 'c) simple -> ('a -> 'd) -> ('d -> 'a) -> ('d, 'b, 'c) simple
  (** Wrap a convertor with the given functions *)

val reader : ('a, 'b, 'c) simple -> ('a, 'b, 'c) Reader.t
  (** Return the reader monad of a type combinator *)

val writer : ('a, 'b, 'c) simple -> 'a -> (unit, 'b, 'c) Writer.t
  (** Return the writer monad of a type combinator *)

val from_wire :
  typ:('b, 'c) annot
  reader:('a, 'b, 'c) Reader.t ->
  writer:('a -> (unit, 'b, 'c) Writer.t) ->
  ('a, 'b, 'c) simple
    (** Create a type combinator from a reader monad, a writer monad
        and the DBus type of the two monads.

        For example a type combinator for tuple can be written like
        that:

        {[
          let triplet x y z =
            from_wire
              ~typ:(annot x @@ annot y @@ annot z)
              ~reader:(perform with module Reader in
                         a <-- reader x;
                         b <-- reader y;
                         c <-- reader z;
                         return (a, b, c))
              ~writer:(fun (a, b, c) ->
                         perform
                            writer x a;
                            writer y b;
                            writer z c)
        ]}
    *)
