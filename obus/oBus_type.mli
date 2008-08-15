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

type context = OBus_internals.connection * string option

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

type ('a, 'b) fold = { fold : 'c. ('a -> 'c -> 'c) -> 'b -> 'c -> 'c }
    (** Polymorphic fold-like function *)

val make_array :
  empty:'b ->
  add:('a -> 'b -> 'b) ->
  fold:('a, 'b) fold -> [< 'a cl_element ] -> 'b ty_single
  (** Create a new array type by providing all necessary functions. *)

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
val tsignature : signature ty_basic
val tobject_path : OBus_path.t ty_basic
val tpath : OBus_path.t ty_basic
val tproxy : OBus_internals.proxy ty_basic

val tlist : [< 'a cl_element ] -> 'a list ty_single
val tset : [< 'a cl_element ] -> 'a list ty_single
  (** [tset] is similar to [tlist] except that it does not conserve
      element order *)
val tdict_entry : [< 'a cl_basic ] -> [< 'b cl_single ] -> ('a * 'b) ty_element
val tassoc : [< 'a cl_basic ] -> [< 'b cl_single ] -> ('a * 'b) list ty_single
  (** [tassoc tk tv] is equivalent to [tset (tdict_entry tk tv)] *)
val tstructure : [< 'a cl_sequence ] -> 'a ty_single
val tvariant : single ty_single

val tbyte_array : string ty_single
  (** Array of bytes seen as string *)

val tpair : [< 'a cl_sequence ] -> [< 'b cl_sequence ] -> ('a * 'b) ty_sequence
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
type proxy = OBus_internals.proxy
type 'a set = 'a list
type ('a, 'b) dict_entry = 'a * 'b
type ('a, 'b) assoc = ('a, 'b) dict_entry set
type 'a structure = 'a
type variant = single
type byte_array = string
    (** Dummy type definition, they should be used in combination with
        the syntax extension, to define the dbus type and the caml
        type at the same time *)

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

(**/**)

val ty_function_send : ('a, 'b, 'c) ty_function -> (OBus_internals.writer -> 'b) -> 'a
val ty_function_recv : ('a, 'b, 'c) ty_function -> ('a -> 'b) OBus_internals.reader
val ty_function_reply_writer : ('a, 'b, 'c) ty_function -> 'c -> OBus_internals.writer
val ty_function_reply_reader : ('a, 'b, 'c) ty_function -> 'c OBus_internals.reader
val ty_writer : [< 'a ty_element | 'a cl_sequence ] -> 'a -> OBus_internals.writer
val ty_reader : [< 'a ty_element | 'a cl_sequence ] -> 'a OBus_internals.reader
