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

open OBus_value

type 'a ty_desc_basic
type 'a ty_desc_single
type 'a ty_desc_element
type 'a ty_desc_sequence
  (** Simple types description *)

type 'a ty_basic = [ `basic of 'a ty_desc_basic ]
type 'a ty_single = [ `single of 'a ty_desc_single ]
type 'a ty_element = [ `element of 'a ty_desc_element ]
type 'a ty_sequence = [ `sequence of 'a ty_desc_sequence ]

type 'a cl_basic = 'a ty_basic
type 'a cl_single = [ 'a cl_basic | 'a ty_single ]
type 'a cl_element = [ 'a cl_single | 'a ty_element ]
type 'a cl_sequence = [ 'a cl_single | 'a ty_sequence ]

type ('a, 'b, 'c) ty_function
  (** Functionnal types *)

(** {6 Dbus types} *)

val isignature : ('a, 'b, 'c) ty_function -> signature
  (** "in" signature, it describe types of the method/signal
      parameters. *)

val osignature : ('a, 'b, 'c) ty_function -> signature
  (** "out" signature, it describe types of return values returned by
      the method. For signals it is [[]]. *)

val type_basic : [< 'a cl_basic ] -> tbasic
val type_single : [< 'a cl_single ] -> tsingle
val type_element : [< 'a cl_element ] -> telement
val type_sequence : [< 'a cl_sequence ] -> tsequence
  (** Return the DBus type of a type description *)

type 'a with_ty_basic = { with_ty_basic : 'b. 'b ty_basic -> 'a }
type 'a with_ty_single = { with_ty_single : 'b. 'b ty_single -> 'a }
type 'a with_ty_element = { with_ty_element : 'b. 'b ty_element -> 'a }
type 'a with_ty_sequence = { with_ty_sequence : 'b. 'b ty_sequence -> 'a }
val with_ty_basic : 'a with_ty_basic -> tbasic -> 'a
val with_ty_single : 'a with_ty_single -> tsingle -> 'a
val with_ty_element : 'a with_ty_element -> telement -> 'a
val with_ty_sequence : 'a with_ty_sequence -> tsequence -> 'a
  (** [with_*_ty func typ] Construct the default combinator for [typ]
      and give it to [func] *)

(** {6 Dynamic values operations} *)

val make_basic : [< 'a cl_basic ] -> 'a -> basic
val make_single : [< 'a cl_single ] -> 'a -> single
val make_element : [< 'a cl_element ] -> 'a -> element
val make_sequence : [< 'a cl_sequence ] -> 'a -> sequence
  (** Make a dynamically typed value from a statically typed one *)

exception Cast_failure
  (** Exception raised when a cast fail *)

type context = exn
    (** The context is used to pass extra data to combinator so they
        can peek extra informations from it. For example to create a
        proxy object we need information from a message header.

        The context used by obus when a message is received on a
        connection is {!OBus_connection.Context}. *)

exception No_context
  (** Context used when no one is specified *)

val cast_basic : [< 'a cl_basic ] -> ?context:context -> basic -> 'a
val cast_single : [< 'a cl_single ] -> ?context:context -> single -> 'a
val cast_element : [< 'a cl_element ] -> ?context:context -> element -> 'a
val cast_sequence : [< 'a cl_sequence ] -> ?context:context -> sequence -> 'a
  (** Cast a dynamically typed value into a statically typed one. It
      raise a [Cast_failure] if types do not match.

      If the type contain [tproxy] or a type derived from [tproxy] you
      must also provide a context. *)

val opt_cast_basic : [< 'a cl_basic ] -> ?context:context -> basic -> 'a option
val opt_cast_single : [< 'a cl_single ] -> ?context:context -> single -> 'a option
val opt_cast_element : [< 'a cl_element ] -> ?context:context -> element -> 'a option
val opt_cast_sequence : [< 'a cl_sequence ] -> ?context:context -> sequence -> 'a option
  (** Same thing but return an option instead of raising an
      exception *)

val make_func : ('a, 'b, 'c) ty_function -> (sequence -> 'b) -> 'a
  (** [make_func typ cont ...] make a sequence from extra parameters
      and pass it to [cont] *)

val cast_func : ('a, 'b, 'c) ty_function -> ?context:context -> sequence -> 'a -> 'b
  (** [cast_func typ seq f] cast [seq] using [typ] and pass the
      resulting values to [f] *)

val opt_cast_func : ('a, 'b, 'c) ty_function -> ?context:context -> sequence -> 'a -> 'b option
  (** Same as [cast_func] but do not raise an exception *)

val func_reply : ('a, 'b, 'c) ty_function -> 'c cl_sequence
  (** Return the return type of a functionnal type *)

(** {6 Types construction} *)

val reply : [< 'a cl_sequence ] -> ('b, 'b, 'a) ty_function
  (** Create a functionnal type from a simple type *)

val abstract : [< 'a cl_sequence ] -> ('b, 'c, 'd) ty_function -> ('a -> 'b, 'c, 'd) ty_function
val ( --> ) : [< 'a cl_sequence ] -> ('b, 'c, 'd) ty_function -> ('a -> 'b, 'c, 'd) ty_function
  (** [abstrant x y] or [x --> y], make an abstraction *)

val wrap_basic : 'a ty_basic -> ('a -> 'b) -> ('b -> 'a) -> 'b ty_basic
val wrap_single : 'a ty_single -> ('a -> 'b) -> ('b -> 'a) -> 'b ty_single
val wrap_element : 'a ty_element -> ('a -> 'b) -> ('b -> 'a) -> 'b ty_element
val wrap_sequence : 'a ty_sequence -> ('a -> 'b) -> ('b -> 'a) -> 'b ty_sequence
  (** Wrap a type description by applying a convertion function *)

val wrap_array : [< 'a cl_element ] ->
  make:(('a -> element) -> 'b -> element list) ->
  cast:((element -> 'a) -> element list -> 'b) -> 'b ty_single
  (** [wrap_array t make cast] wrap an array type. It more efficient
      than a [wrap_single (tlist t) ...] since it does not create an
      intermediate list. *)

val wrap_basic_ctx : 'a ty_basic -> (context -> 'a -> 'b) -> ('b -> 'a) -> 'b ty_basic
val wrap_single_ctx : 'a ty_single -> (context -> 'a -> 'b) -> ('b -> 'a) -> 'b ty_single
val wrap_element_ctx : 'a ty_element -> (context -> 'a -> 'b) -> ('b -> 'a) -> 'b ty_element
val wrap_sequence_ctx : 'a ty_sequence -> (context -> 'a -> 'b) -> ('b -> 'a) -> 'b ty_sequence
val wrap_array_ctx : [< 'a cl_element ] ->
  make:(('a -> element) -> 'b -> element list) ->
  cast:(context -> (element -> 'a) -> element list -> 'b) -> 'b ty_single
  (** Same thing but with access to the context *)

(** {6 Default type combinators} *)

module Pervasives : sig

  (** This module is automatically opened by the syntax extension *)

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
  val tsignature : signature ty_basic
  val tobject_path : OBus_path.t ty_basic
  val tpath : OBus_path.t ty_basic

  val tlist : [< 'a cl_element ] -> 'a list ty_single
  val tdict_entry : [< 'a cl_basic ] -> [< 'b cl_single ] -> ('a * 'b) ty_element
  val tassoc : [< 'a cl_basic ] -> [< 'b cl_single ] -> ('a * 'b) list ty_single
    (** [tassoc tk tv] is equivalent to [tlist (tdict_entry tk tv)] *)
  val tstructure : [< 'a cl_sequence ] -> 'a ty_single
  val tvariant : single ty_single

  val tbyte_array : string ty_single
    (** Array of bytes seen as string *)

  val tunit : unit ty_sequence

  type byte = char
  type boolean = bool
  type int8 = int
  type uint8 = int
  type int16 = int
  type uint16 = int
  type uint32 = int32
  type uint64 = int64
  type uint = int
  type double = float
  type signature = OBus_value.signature
  type object_path = OBus_path.t
  type path = OBus_path.t
  type ('a, 'b) dict_entry = 'a * 'b
  type ('a, 'b) assoc = ('a, 'b) dict_entry list
  type 'a structure = 'a
  type variant = single
  type byte_array = string
      (** Dummy type definition, they should be used in combination with
          the syntax extension, to define the dbus type and the caml
          type at the same time *)

end

(** {6 map and set with obus type} *)

module type Ordered_element_type = sig
  type t
  val tt : t cl_element
  val compare : t -> t -> int
end

module Make_set(Ord : Ordered_element_type) : sig
  include Set.S with type elt = Ord.t
  val tt : t ty_single
end

module type Ordered_basic_type = sig
  type t
  val tt : t cl_basic
  val compare : t -> t -> int
end

module Make_map(Ord : Ordered_basic_type) : sig
  include Map.S with type key = Ord.t
  val tt : [< 'a cl_single ] -> 'a t ty_single
end

(** {6 Tuples} *)

val tup2 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  ('a1 * 'a2) ty_sequence
val tup3 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  [< 'a3 cl_sequence ] ->
  ('a1 * 'a2 * 'a3) ty_sequence
val tup4 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  [< 'a3 cl_sequence ] ->
  [< 'a4 cl_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4) ty_sequence
val tup5 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  [< 'a3 cl_sequence ] ->
  [< 'a4 cl_sequence ] ->
  [< 'a5 cl_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5) ty_sequence
val tup6 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  [< 'a3 cl_sequence ] ->
  [< 'a4 cl_sequence ] ->
  [< 'a5 cl_sequence ] ->
  [< 'a6 cl_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) ty_sequence
val tup7 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  [< 'a3 cl_sequence ] ->
  [< 'a4 cl_sequence ] ->
  [< 'a5 cl_sequence ] ->
  [< 'a6 cl_sequence ] ->
  [< 'a7 cl_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) ty_sequence
val tup8 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  [< 'a3 cl_sequence ] ->
  [< 'a4 cl_sequence ] ->
  [< 'a5 cl_sequence ] ->
  [< 'a6 cl_sequence ] ->
  [< 'a7 cl_sequence ] ->
  [< 'a8 cl_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) ty_sequence
val tup9 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  [< 'a3 cl_sequence ] ->
  [< 'a4 cl_sequence ] ->
  [< 'a5 cl_sequence ] ->
  [< 'a6 cl_sequence ] ->
  [< 'a7 cl_sequence ] ->
  [< 'a8 cl_sequence ] ->
  [< 'a9 cl_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) ty_sequence
val tup10 :
  [< 'a1 cl_sequence ] ->
  [< 'a2 cl_sequence ] ->
  [< 'a3 cl_sequence ] ->
  [< 'a4 cl_sequence ] ->
  [< 'a5 cl_sequence ] ->
  [< 'a6 cl_sequence ] ->
  [< 'a7 cl_sequence ] ->
  [< 'a8 cl_sequence ] ->
  [< 'a9 cl_sequence ] ->
  [< 'a10 cl_sequence ] ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) ty_sequence
