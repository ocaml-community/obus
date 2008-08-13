(*
 * oBus_type.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** OBus type combinators *)

(** This modules define type combinators which must be used to
    describe the type of methods, signals and properties or to
    make/cast dynamically typed values.

    The [pa_obus] syntax extension allow to write these combinators as
    caml types.
*)

type 'a ty_desc_basic
type 'a ty_desc_single
type 'a ty_desc_sequence
  (** Simple types description *)

type 'a ty_basic = [ `basic of 'a ty_desc_basic ]
type 'a ty_single = [ 'a ty_basic | `single of 'a ty_desc_single ]
type 'a ty_sequence = [ 'a ty_single | `sequence of 'a ty_desc_sequence ]

type ('a, 'b, 'c) ty_function
  (** Functionnal types *)

(** {6 Dbus types} *)

val isignature : ('a, 'b, 'c) ty_function -> OBus_value.signature
  (** "in" signature, it describe types of the method/signal
      parameters. *)

val osignature : ('a, 'b, 'c) ty_function -> OBus_value.signature
  (** "out" signature, it describe types of return values returned by
      the method. For signals it is [[]]. *)

val type_basic : [< 'a ty_basic ] -> OBus_value.tbasic
val type_single : [< 'a ty_single ] -> OBus_value.tsingle
val type_sequence : [< 'a ty_sequence ] -> OBus_value.tsequence
  (** Return the DBus type of a type description *)

type 'a with_basic_ty = { with_basic_ty : 'b. 'b ty_basic -> 'a }
type 'a with_single_ty = { with_single_ty : 'b. 'b ty_single -> 'a }
type 'a with_sequence_ty = { with_sequence_ty : 'b. 'b ty_sequence -> 'a }
val with_basic_ty : 'a with_basic_ty -> OBus_value.tbasic -> 'a
val with_single_ty : 'a with_single_ty -> OBus_value.tsingle -> 'a
val with_sequence_ty : 'a with_sequence_ty -> OBus_value.tsequence -> 'a
  (** [with_*_ty func typ] Construct the default class for [typ] and
      give it to [func] *)

(** {6 Dynamic values operations} *)

val make_basic : [< 'a ty_basic ] -> 'a -> OBus_value.basic
val make_single : [< 'a ty_single ] -> 'a -> OBus_value.single
val make_sequence : [< 'a ty_sequence ] -> 'a -> OBus_value.sequence
  (** Make a dynamically typed value from a statically typed one *)

exception Cast_failure
  (** Exception raised when a cast fail *)

val cast_basic : [< 'a ty_basic ] -> OBus_value.basic -> 'a
val cast_single : [< 'a ty_single ] -> OBus_value.single -> 'a
val cast_sequence : [< 'a ty_sequence ] -> OBus_value.sequence -> 'a
  (** Cast a dynamically typed value into a statically typed one. It
      raise a [Cast_failure] if types do not match *)

val opt_cast_basic : [< 'a ty_basic ] -> OBus_value.basic -> 'a option
val opt_cast_single : [< 'a ty_single ] -> OBus_value.single -> 'a option
val opt_cast_sequence : [< 'a ty_sequence ] -> OBus_value.sequence -> 'a option
  (** Same thing but return an option instead of raising an
      exception *)

(** {6 Types construction} *)

val reply : [< 'a ty_sequence ] -> ('b, 'b, 'a) ty_function
  (** Create a functionnal type from a simple type *)

val abstract : [< 'a ty_sequence ] -> ('b, 'c, 'd) ty_function -> ('a -> 'b, 'c, 'd) ty_function
val ( --> ) : [< 'a ty_sequence ] -> ('b, 'c, 'd) ty_function -> ('a -> 'b, 'c, 'd) ty_function
  (** [abstrant x y] or [x --> y], make an abstraction *)

val wrap_basic : [< 'a ty_basic ] -> ('a -> 'b) -> ('b -> 'a) -> 'b ty_basic
val wrap_single : [< 'a ty_single ] -> ('a -> 'b) -> ('b -> 'a) -> 'b ty_single
val wrap_sequence : [< 'a ty_sequence ] -> ('a -> 'b) -> ('b -> 'a) -> 'b ty_sequence
  (** Wrap a type description by applying a convertion function *)

(** {6 Default type combinators} *)

val tbyte : char ty_basic
val tchar : char ty_basic
val tboolean : bool ty_basic
val tbool : bool ty_basic
val tint8 : int ty_basic
val tint16 : int ty_basic
val tint : int ty_basic
val tint32 : int32 ty_basic
val tint64 : int64 ty_basic
val tuint8 : int ty_basic
val tuint16 : int ty_basic
val tuint : int ty_basic
val tuint32 : int32 ty_basic
val tuint64 : int64 ty_basic
val tdouble : float ty_basic
val tfloat : float ty_basic
val tstring : string ty_basic
val tsignature : OBus_value.signature ty_basic
val tobject_path : OBus_path.t ty_basic
val tpath : OBus_path.t ty_basic
val tproxy : OBus_internals.proxy ty_basic

val tlist : [< 'a ty_single ] -> 'a list ty_single
val tset : [< 'a ty_single ] -> 'a list ty_single
  (** [tset] is similar to [tlist] except that it does not conserve
      element order *)
val tassoc : [< 'a ty_basic ] -> [< 'b ty_single ] -> ('a * 'b) list ty_single
val tstructure : [< 'a ty_sequence ] -> 'a ty_single
val tvariant : OBus_value.single ty_single

val tbyte_array : string ty_single
  (** Array of bytes seen as string *)

val tpair : [< 'a ty_sequence ] -> [< 'b ty_sequence ] -> ('a * 'b) ty_sequence
val tunit : unit ty_sequence

(** {6 Tuples} *)

val tup2 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  ('a1 * 'a2) ty_sequence
val tup3 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  [< 'a3 ty_sequence ] ->
  ('a1 * 'a2 * 'a3) ty_sequence
val tup4 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  [< 'a3 ty_sequence ] ->
  [< 'a4 ty_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4) ty_sequence
val tup5 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  [< 'a3 ty_sequence ] ->
  [< 'a4 ty_sequence ] ->
  [< 'a5 ty_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5) ty_sequence
val tup6 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  [< 'a3 ty_sequence ] ->
  [< 'a4 ty_sequence ] ->
  [< 'a5 ty_sequence ] ->
  [< 'a6 ty_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) ty_sequence
val tup7 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  [< 'a3 ty_sequence ] ->
  [< 'a4 ty_sequence ] ->
  [< 'a5 ty_sequence ] ->
  [< 'a6 ty_sequence ] ->
  [< 'a7 ty_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) ty_sequence
val tup8 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  [< 'a3 ty_sequence ] ->
  [< 'a4 ty_sequence ] ->
  [< 'a5 ty_sequence ] ->
  [< 'a6 ty_sequence ] ->
  [< 'a7 ty_sequence ] ->
  [< 'a8 ty_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) ty_sequence
val tup9 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  [< 'a3 ty_sequence ] ->
  [< 'a4 ty_sequence ] ->
  [< 'a5 ty_sequence ] ->
  [< 'a6 ty_sequence ] ->
  [< 'a7 ty_sequence ] ->
  [< 'a8 ty_sequence ] ->
  [< 'a9 ty_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) ty_sequence
val tup10 :
  [< 'a1 ty_sequence ] ->
  [< 'a2 ty_sequence ] ->
  [< 'a3 ty_sequence ] ->
  [< 'a4 ty_sequence ] ->
  [< 'a5 ty_sequence ] ->
  [< 'a6 ty_sequence ] ->
  [< 'a7 ty_sequence ] ->
  [< 'a8 ty_sequence ] ->
  [< 'a9 ty_sequence ] ->
  [< 'a10 ty_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) ty_sequence

(**/**)

val ty_function_send : ('a, 'b, 'c) ty_function -> (OBus_internals.writer -> 'b) -> 'a
val ty_function_recv : ('a, 'b, 'c) ty_function -> ('a -> 'b) OBus_internals.reader
val ty_function_reply_writer : ('a, 'b, 'c) ty_function -> 'c -> OBus_internals.writer
val ty_function_reply_reader : ('a, 'b, 'c) ty_function -> 'c OBus_internals.reader
val ty_writer : [< 'a ty_sequence ] -> 'a -> OBus_internals.writer
val ty_reader : [< 'a ty_sequence ] -> 'a OBus_internals.reader
