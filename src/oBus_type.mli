(*
 * oBus_type.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** OBus type combinators *)

(** This modules define type combinators which must be used to
    describe the type of methods, signals and properties or to
    make/cast dynamically typed values.

    The [pa_obus] syntax extension allow to write these combinators as
    caml types.
*)

type ('a, 'cl) t = ('a, 'cl) OBus_private_type.t
  (** Type of type combinators. *)

type 'a basic = ('a, [`Basic]) t
type 'a container = ('a, [`Container]) t
type 'a sequence = ('a, [`Sequence]) t

type ('a, 'cl) cl_basic = ('a, 'cl) t
constraint 'cl = [ `Basic ]
    (** Type matching type of the basic class *)

type ('a, 'cl) cl_single = ('a, 'cl) t
constraint 'cl = [< `Basic | `Container ]
    (** Type matching type of the single class *)

type ('a, 'cl) cl_sequence = ('a, 'cl) t
constraint 'cl = [< `Basic | `Container | `Sequence ]
    (** Type matching type of the sequence class *)

type ('a, 'b, 'c) func
  (** Functionnal types *)

type context = OBus_connection.t * OBus_message.t
    (** The context is used to pass extra informations to
        combinators. See {!OBus_connection.Context}. *)

(** {6 D-Bus types} *)

val isignature : ('a, 'b, 'c) func -> OBus_value.signature
  (** "in" signature, it describe types of the method/signal
      parameters. *)

val osignature : ('a, 'b, 'c) func -> OBus_value.signature
  (** "out" signature, it describe types of return values returned by
      the method. For signals it is [[]]. *)

val type_basic : ('a, _) cl_basic -> OBus_value.tbasic
val type_single : ('a, _) cl_single -> OBus_value.tsingle
val type_sequence : ('a, _) cl_sequence -> OBus_value.tsequence
  (** Return the D-Bus type of a type description *)

(** {6 Dynamic values operations} *)

val make_basic : ('a, _) cl_basic -> 'a -> OBus_value.basic
val make_single : ('a, _) cl_single -> 'a -> OBus_value.single
val make_sequence : ('a, _) cl_sequence -> 'a -> OBus_value.sequence
  (** Make a dynamically typed value from a statically typed one *)

exception Cast_failure of string * string
  (** Exception raised when a cast fail. Arguments are:
      - the function which raised the exception
      - an error message *)

val cast_basic : ('a, _) cl_basic -> ?context : context -> OBus_value.basic -> 'a
val cast_single : ('a, _) cl_single -> ?context : context -> OBus_value.single -> 'a
val cast_sequence : ('a, _) cl_sequence -> ?context : context -> OBus_value.sequence -> 'a
  (** Casts a dynamically typed value into a statically typed one. It
      raises a [Cast_failure] if types do not match. *)

val make_func : ('a, 'b, 'c) func -> (OBus_value.sequence -> 'b) -> 'a
  (** [make_func typ cont ...] makes a sequence from extra parameters
      and pass it to [cont] *)

val cast_func : ('a, 'b, 'c) func -> ?context : context -> OBus_value.sequence -> ('a -> 'b)
  (** [cast_func typ context seq] casts [seq] using [typ] and returns
      a function which, when applied on a function, pass it the
      resulting values. *)

val func_reply : ('a, 'b, 'c) func -> 'c sequence
  (** Return the return type of a functionnal type *)

(** {6 Types construction} *)

val reply : ('a, _) cl_sequence -> ('b, 'b, 'a) func
  (** Create a functionnal type from a simple type *)

val abstract : ('a, _) cl_sequence -> ('b, 'c, 'd) func -> ('a -> 'b, 'c, 'd) func
val ( --> ) : ('a, _) cl_sequence -> ('b, 'c, 'd) func -> ('a -> 'b, 'c, 'd) func
  (** [abstrant x y] or [x --> y], make an abstraction *)

val map : ('a, 'cl) t -> ('a -> 'b) -> ('b -> 'a) -> ('b, 'cl) t
  (** Map a type description by applying a convertion function *)

val map_with_context : ('a, 'cl) t -> (context -> 'a -> 'b) -> ('b -> 'a) -> ('b, 'cl) t
  (** Same thing but with access to the context *)

(** {6 Helpers} *)

val mapping : ('a, 'cl) t -> ('b * 'a) list -> ('b, 'cl) t
  (** Create a type combinator from another one and a value
      mapping. *)

val bitwise : (int, 'cl) t -> ('a * int) list -> ('a list, 'cl) t
val bitwise32 : (int32, 'cl) t -> ('a * int) list -> ('a list, 'cl) t
val bitwise64 : (int64, 'cl) t -> ('a * int) list -> ('a list, 'cl) t

(** {6 Tuples} *)

val tuple2 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a1 * 'a2) sequence
val tuple3 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a3, _) cl_sequence ->
  ('a1 * 'a2 * 'a3) sequence
val tuple4 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a3, _) cl_sequence ->
  ('a4, _) cl_sequence ->
  ('a1 * 'a2 * 'a3 * 'a4) sequence
val tuple5 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a3, _) cl_sequence ->
  ('a4, _) cl_sequence ->
  ('a5, _) cl_sequence ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5) sequence
val tuple6 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a3, _) cl_sequence ->
  ('a4, _) cl_sequence ->
  ('a5, _) cl_sequence ->
  ('a6, _) cl_sequence ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) sequence
val tuple7 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a3, _) cl_sequence ->
  ('a4, _) cl_sequence ->
  ('a5, _) cl_sequence ->
  ('a6, _) cl_sequence ->
  ('a7, _) cl_sequence ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) sequence
val tuple8 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a3, _) cl_sequence ->
  ('a4, _) cl_sequence ->
  ('a5, _) cl_sequence ->
  ('a6, _) cl_sequence ->
  ('a7, _) cl_sequence ->
  ('a8, _) cl_sequence ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) sequence
val tuple9 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a3, _) cl_sequence ->
  ('a4, _) cl_sequence ->
  ('a5, _) cl_sequence ->
  ('a6, _) cl_sequence ->
  ('a7, _) cl_sequence ->
  ('a8, _) cl_sequence ->
  ('a9, _) cl_sequence ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) sequence
val tuple10 :
  ('a1, _) cl_sequence ->
  ('a2, _) cl_sequence ->
  ('a3, _) cl_sequence ->
  ('a4, _) cl_sequence ->
  ('a5, _) cl_sequence ->
  ('a6, _) cl_sequence ->
  ('a7, _) cl_sequence ->
  ('a8, _) cl_sequence ->
  ('a9, _) cl_sequence ->
  ('a10, _) cl_sequence ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) sequence
