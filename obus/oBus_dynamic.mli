(*
 * oBus_dynamic.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Dynamically typed DBus values *)

open OBus_types

type 'a t
  (** Type of a dynamic value of DBus type class ['a]. ['a] is one of
      [OBus_types.basic], [OBus_types.single], [OBus_types.sequence] *)

type variant = single t
    (** One single dynamically typed value *)

type body = sequence t
    (** A sequence of dynamically typed values. Represent the body of
        a message when evalued dynamically. *)

(** This part is basically an emulation of what we can do with
    GADTs *)

type (+'a, +'cl) ty
  (** Typed type *)

val make : ('a, 'b) ty -> 'a -> 'b t
  (** Dynamic value construction *)

val get : ('a, 'b) ty -> 'b t -> 'a
  (** Dynamic value deconstruction. Raise a [Failure] if types does
      match *)

val typ : 'a t -> 'a
  (** Return a dynamic representation of the type of a dynamic
      value *)

val dynamic_ty : ('a, 'cl) ty -> 'cl
  (** Rerutn a dynamic representation of a typed DBus type *)

type ('cl, 'b) with_ty = { with_ty : 'a. ('a, 'cl) ty -> 'b }

val with_ty : ('cl, 'b) with_ty -> 'cl -> 'b
  (** Obtain a typed representation of a DBus type from a dynamic
      one. For example:

      {[
           match typ v with
             | Tarray t -> with_ty { with_ty = fun t -> List.length (get (tarray t) v) } t
             ...
      ]}
  *)

type ('cl, 'b) with_content = { with_content : 'a. ('a, 'cl) ty -> 'a -> 'b }

val with_content : ('cl, 'b) with_content -> 'cl t -> 'b
  (** Apply a function on the content of a variant *)

val with_basic : < match_basic : 'a. ('a, basic) ty -> 'a -> 'b; .. > -> basic t -> 'b
val with_single : < match_single : 'a. ('a, single) ty -> 'a -> 'b; .. > -> single t -> 'b
val with_sequence : < match_sequence : 'a. ('a, sequence) ty -> 'a -> 'b; .. > -> sequence t -> 'b
  (** Same thing but with object, this allow to use directly matching
      objects *)

val with_basic_ty : < match_basic : 'a. ('a, basic) ty -> 'b; .. > -> basic -> 'b
val with_single_ty : < match_single : 'a. ('a, single) ty -> 'b; .. > -> single -> 'b
val with_sequence_ty : < match_sequence : 'a. ('a, sequence) ty -> 'b; .. > -> sequence -> 'b

(** {6 Pretty-printing} *)

val string_of_basic : basic t -> string
val string_of_single : single t -> string
val string_of_sequence : sequence t -> string

(** {6 Typed constructors} *)

val tbyte : (char, basic) ty
val tboolean : (bool, basic) ty
val tint16 : (int, basic) ty
val tint32 : (int32, basic) ty
val tint64 : (int64, basic) ty
val tuint16 : (int, basic) ty
val tuint32 : (int32, basic) ty
val tuint64 : (int64, basic) ty
val tdouble : (float, basic) ty
val tstring : (string, basic) ty
val tsignature : (signature, basic) ty
val tobject_path : (OBus_path.t, basic) ty
val tbasic : ('a, basic) ty -> ('a, single) ty
val tarray : ('a, single) ty -> ('a list, single) ty
val tdict : ('a, basic) ty -> ('b, single) ty -> (('a * 'b) list, single) ty
val tstruct : ('a, sequence) ty -> ('a, single) ty
val tvariant : (variant, single) ty
val tcons : ('a, single) ty -> ('b, sequence) ty -> ('a * 'b, sequence) ty
val tnil : (unit, sequence) ty

(** {6 Matching} *)

module type Matcher = sig
  type ('a, 'cl) branch

  class matcher : object
    method default_basic : 'a. ('a, basic) ty -> ('a, basic) branch
    method default_single : 'a. ('a, single) ty -> ('a, single) branch
    method default_sequence : 'a. ('a, sequence) ty -> ('a, sequence) branch

    method byte : (char, basic) branch
    method boolean : (bool, basic) branch
    method int16 : (int, basic) branch
    method int32 : (int32, basic) branch
    method int64 : (int64, basic) branch
    method uint16 : (int, basic) branch
    method uint32 : (int32, basic) branch
    method uint64 : (int64, basic) branch
    method double : (float, basic) branch
    method string : (string, basic) branch
    method signature : (signature, basic) branch
    method object_path : (OBus_path.t, basic) branch

    method basic : 'a. ('a, basic) ty -> ('a, single) branch
    method array : 'a. ('a, single) ty -> ('a list, single) branch
    method dict : 'a 'b. ('a, basic) ty -> ('b, single) ty -> (('a * 'b) list, single) branch
    method structure : 'a. ('a, sequence) ty -> ('a, single) branch
    method variant : (variant, single) branch

    method cons : 'a 'b. ('a, single) ty -> ('b, sequence) ty -> ('a * 'b, sequence) branch
    method nil : (unit, sequence) branch

    method match_basic : 'a. ('a, basic) ty -> ('a, basic) branch
    method match_single : 'a. ('a, single) ty -> ('a, single) branch
    method match_sequence : 'a. ('a, sequence) ty -> ('a, sequence) branch
  end
end

module Matcher(Branch : sig type ('a, 'cl) t end) : Matcher
  with type ('a, 'cl) branch = ('a, 'cl) Branch.t

(** {6 Combinators} *)

(** The idea is that if you want to write a fold/map or any function
    which can be applied on any dynamically typed value, we first need
    to construct the function from the type of the value and then to
    apply it on the value itself.

    So you have to provide a set of combinators (one by construction)
    from which will be constructed a function which can produce a
    combinator for any dynamic type.

    For example a value printer can be written we the given set of
    combinators:

    module Printers = struct
      type 'a t = 'a -> string

      let cbyte = sprintf "%C"
      let cint32 = sprintf "%ld"
      let carray p l = String.concat "; " (List.map p l)
      ...
    end
*)

module type Combinators = sig
  type ('a, 'cl) t

  val cbyte : (char, basic) t
  val cboolean : (bool, basic) t
  val cint16 : (int, basic) t
  val cint32 : (int32, basic) t
  val cint64 : (int64, basic) t
  val cuint16 : (int, basic) t
  val cuint32 : (int32, basic) t
  val cuint64 : (int64, basic) t
  val cdouble : (float, basic) t
  val cstring : (string, basic) t
  val csignature : (signature, basic) t
  val cobject_path : (OBus_path.t, basic) t
  val cbasic : ('a, basic) t -> ('a, single) t
  val carray : ('a, single) t -> ('a list, single) t
  val cdict : ('a, basic) t -> ('b, single) t -> (('a * 'b) list, single) t
  val cstruct : ('a, sequence) t -> ('a, single) t
  val cvariant : ('a, single) t -> (('a, single) ty * 'a, single) t
  val ccons : ('a, single) t -> ('b, sequence) t -> ('a * 'b, sequence) t
  val cnil : (unit, sequence) t
end

module Maker(Combinators : Combinators) : sig
  val make_basic : ('a, basic) ty -> ('a, basic) Combinators.t
  val make_single : ('a, single) ty -> ('a, single) Combinators.t
  val make_sequence : ('a, sequence) ty -> ('a, sequence) Combinators.t

  class make : object
    method match_basic : 'a. ('a, basic) ty -> ('a, basic) Combinators.t
    method match_single : 'a. ('a, single) ty -> ('a, single) Combinators.t
    method match_sequence : 'a. ('a, sequence) ty -> ('a, sequence) Combinators.t
  end
end

(** {3 Unsafe operations} *)

type 'a unsafe = 'a * Obj.t

val of_unsafe : 'a unsafe -> 'a t
val to_unsafe : 'a t -> 'a unsafe
