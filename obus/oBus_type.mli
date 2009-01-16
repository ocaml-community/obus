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

type ('a, 'cl) t
  (** Type of type combinators *)

type 'a basic = ('a, [`basic]) t
type 'a single = ('a, [`single]) t
type 'a element = ('a, [`element]) t
type 'a sequence = ('a, [`sequence]) t

type ('a, 'cl) cl_basic = ('a, 'cl) t
constraint 'cl = [ `basic ]

type ('a, 'cl) cl_single = ('a, 'cl) t
constraint 'cl = [< `basic | `single ]

type ('a, 'cl) cl_element = ('a, 'cl) t
constraint 'cl = [< `basic | `single | `element ]

type ('a, 'cl) cl_sequence = ('a, 'cl) t
constraint 'cl = [< `basic | `single | `sequence ]

type ('a, 'b, 'c) func
  (** Functionnal types *)

(** {6 DBus types} *)

val isignature : ('a, 'b, 'c) func -> OBus_value.signature
  (** "in" signature, it describe types of the method/signal
      parameters. *)

val osignature : ('a, 'b, 'c) func -> OBus_value.signature
  (** "out" signature, it describe types of return values returned by
      the method. For signals it is [[]]. *)

val type_basic : ('a, _) cl_basic -> OBus_value.tbasic
val type_single : ('a, _) cl_single -> OBus_value.tsingle
val type_element : ('a, _) cl_element -> OBus_value.telement
val type_sequence : ('a, _) cl_sequence -> OBus_value.tsequence
  (** Return the DBus type of a type description *)

type 'a with_basic = { with_basic : 'b. 'b basic -> 'a }
type 'a with_single = { with_single : 'b. 'b single -> 'a }
type 'a with_element = { with_element : 'b. 'b element -> 'a }
type 'a with_sequence = { with_sequence : 'b. 'b sequence -> 'a }
val with_basic : 'a with_basic -> OBus_value.tbasic -> 'a
val with_single : 'a with_single -> OBus_value.tsingle -> 'a
val with_element : 'a with_element -> OBus_value.telement -> 'a
val with_sequence : 'a with_sequence -> OBus_value.tsequence -> 'a
  (** [with_*_ty func typ] Construct the default combinator for [typ]
      and give it to [func] *)

(** {6 Dynamic values operations} *)

val make_basic : ('a, _) cl_basic -> 'a -> OBus_value.basic
val make_single : ('a, _) cl_single -> 'a -> OBus_value.single
val make_element : ('a, _) cl_element -> 'a -> OBus_value.element
val make_sequence : ('a, _) cl_sequence -> 'a -> OBus_value.sequence
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

val cast_basic : ('a, _) cl_basic -> ?context:context -> OBus_value.basic -> 'a
val cast_single : ('a, _) cl_single -> ?context:context -> OBus_value.single -> 'a
val cast_element : ('a, _) cl_element -> ?context:context -> OBus_value.element -> 'a
val cast_sequence : ('a, _) cl_sequence -> ?context:context -> OBus_value.sequence -> 'a
  (** Cast a dynamically typed value into a statically typed one. It
      raise a [Cast_failure] if types do not match.

      If the type contain [tproxy] or a type derived from [tproxy] you
      must also provide a context. *)

val opt_cast_basic : ('a, _) cl_basic -> ?context:context -> OBus_value.basic -> 'a option
val opt_cast_single : ('a, _) cl_single -> ?context:context -> OBus_value.single -> 'a option
val opt_cast_element : ('a, _) cl_element -> ?context:context -> OBus_value.element -> 'a option
val opt_cast_sequence : ('a, _) cl_sequence -> ?context:context -> OBus_value.sequence -> 'a option
  (** Same thing but return an option instead of raising an
      exception *)

val make_func : ('a, 'b, 'c) func -> (OBus_value.sequence -> 'b) -> 'a
  (** [make_func typ cont ...] make a sequence from extra parameters
      and pass it to [cont] *)

val cast_func : ('a, 'b, 'c) func -> ?context:context -> OBus_value.sequence -> 'a -> 'b
  (** [cast_func typ seq f] cast [seq] using [typ] and pass the
      resulting values to [f] *)

val opt_cast_func : ('a, 'b, 'c) func -> ?context:context -> OBus_value.sequence -> 'a -> 'b option
  (** Same as [cast_func] but do not raise an exception *)

val func_reply : ('a, 'b, 'c) func -> 'c sequence
  (** Return the return type of a functionnal type *)

(** {6 Types construction} *)

val reply : ('a, _) cl_sequence -> ('b, 'b, 'a) func
  (** Create a functionnal type from a simple type *)

val abstract : ('a, _) cl_sequence -> ('b, 'c, 'd) func -> ('a -> 'b, 'c, 'd) func
val ( --> ) : ('a, _) cl_sequence -> ('b, 'c, 'd) func -> ('a -> 'b, 'c, 'd) func
  (** [abstrant x y] or [x --> y], make an abstraction *)

val wrap : ('a, 'cl) t -> ('a -> 'b) -> ('b -> 'a) -> ('b, 'cl) t
  (** Wrap a type description by applying a convertion function *)

val wrap_array : ('a, _) cl_element ->
  make:(('a -> OBus_value.element) -> 'b -> OBus_value.element list) ->
  cast:((OBus_value.element -> 'a) -> OBus_value.element list -> 'b) -> 'b single
  (** [wrap_array t make cast] wrap an array type. It more efficient
      than a [wrap (tlist t) ...] since it does not create an
      intermediate list. *)

val wrap_with_context : ('a, 'cl) t -> (context -> 'a -> 'b) -> ('b -> 'a) -> ('b, 'cl) t
val wrap_array_with_context : ('a, _) cl_element ->
  make:(('a -> OBus_value.element) -> 'b -> OBus_value.element list) ->
  cast:(context -> (OBus_value.element -> 'a) -> OBus_value.element list -> 'b) -> 'b single
  (** Same thing but with access to the context *)

(** {6 Helpers} *)

val map : ('a, 'cl) t -> ('b * 'a) list -> ('b, 'cl) t
  (** Create a type combinator from another one and a value
      mapping. *)

val bitwise : (int, 'cl) t -> ('a * int) list -> ('a list, 'cl) t
val bitwise32 : (int32, 'cl) t -> ('a * int) list -> ('a list, 'cl) t
val bitwise64 : (int64, 'cl) t -> ('a * int) list -> ('a list, 'cl) t

(** {6 Default type combinators} *)

module Pervasives : sig

  (** This module is automatically opened by the syntax extension *)

  val obus_byte : char basic
  val obus_char : char basic
  val obus_boolean : bool basic
  val obus_bool : bool basic
  val obus_int8 : int basic
  val obus_int16 : int basic
  val obus_int : int basic
  val obus_int32 : int32 basic
  val obus_int64 : int64 basic
  val obus_uint8 : int basic
  val obus_uint16 : int basic
  val obus_uint : int basic
  val obus_uint32 : int32 basic
  val obus_uint64 : int64 basic
  val obus_double : float basic
  val obus_float : float basic
  val obus_string : string basic
  val obus_signature : OBus_value.signature basic
  val obus_object_path : OBus_path.t basic
  val obus_path : OBus_path.t basic

  val obus_list : ('a, _) cl_element -> 'a list single
  val obus_dict_entry : ('a, _) cl_basic -> ('b, _) cl_single -> ('a * 'b) element
  val obus_assoc : ('a, _) cl_basic -> ('b, _) cl_single -> ('a * 'b) list single
    (** [tassoc tk tv] is equivalent to [tlist (tdict_entry tk tv)] *)
  val obus_structure : ('a, _) cl_sequence -> 'a single
  val obus_variant : OBus_value.single single

  val obus_byte_array : string single
    (** Array of bytes seen as string *)

  val obus_unit : unit sequence

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
  type variant = OBus_value.single
  type byte_array = string
      (** Dummy type definition, they should be used in combination with
          the syntax extension, to define the dbus type and the caml
          type at the same time *)

end

(** {6 map and set with obus type} *)

module type Ordered_element_type = sig
  type t
  val obus_t : (t, _) cl_element
  val compare : t -> t -> int
end

module Make_set(Ord : Ordered_element_type) : sig
  include Set.S with type elt = Ord.t
  val obus_t : t single
end

module type Ordered_basic_type = sig
  type t
  val obus_t : (t, _) cl_basic
  val compare : t -> t -> int
end

module Make_map(Ord : Ordered_basic_type) : sig
  include Map.S with type key = Ord.t
  val obus_t : ('a, _) cl_single -> 'a t single
end

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
