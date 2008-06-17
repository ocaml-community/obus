(*
 * values.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** {6 Representation and manipulations of dbus values} *)

type t =
    private
  | Byte of char
  | Boolean of bool
  | Int16 of int
  | Int32 of int32
  | Int64 of int64
  | Uint16 of int
  | Uint32 of int32
  | Uint64 of int64
  | Double of float
  | String of string
  | Signature of Types.t list
  | Object_path of Path.t
  | Array of Types.t * t list
  | Dict of Types.basic  * Types.t * (t * t) list
      (** Array and dict must also contain types information because
          they can be empty *)
  | Structure of t list
  | Variant of t

val to_string : t -> string
  (** Print a caml-style representation of a value *)

val typ : t -> Types.t
  (** Return the type of a value *)

(** {6 Construction of values} *)

(** All the following functions raise an [Invalid_argument] if you try
    to construct incorrect values. Incorrect values are arrays and
    dictionnaries when the elements have not all the same type *)

val byte : char -> t
val boolean : bool -> t
val int16 : int -> t
val int32 : int32 -> t
val int64 : int64 -> t
val uint16 : int -> t
val uint32 : int32 -> t
val uint64 : int64 -> t
val double : float -> t
val string : string -> t
val signature : Types.t list -> t
val object_path : Path.t -> t
val array : Types.t -> t list -> t
val dict : Types.basic -> Types.t -> (t * t) list -> t
val structure : t list -> t
val variant : t -> t

(** {6 Safe constructions DBus values} *)

module Safe : sig
  type 'a basic
  type 'a single
  type 'a seq

  val byte : char basic
  val boolean : bool basic
  val int16 : int basic
  val int32 : int32 basic
  val int64 : int64 basic
  val uint16 : int basic
  val uint32 : int32 basic
  val uint64 : int64 basic
  val double : float basic
  val string : string basic
  val signature : Types.t list basic
  val object_path : Path.t basic

  val basic : 'a basic -> 'a single
  val array : 'a single -> 'a list single
  val dict : 'a basic -> 'b single -> ('a * 'b) list single
  val structure : 'a seq  -> 'a single
  val variant : t single

  val cons : 'a single -> 'b seq -> ('a * 'b) seq
  val nil : unit seq

  val make_single : 'a single -> 'a -> t
  val make : 'a seq -> 'a -> t list

  val get_single : 'a single -> t -> 'a
  val get : 'a seq -> t list -> 'a
    (** [get] and [get_single] raise an [Invalid_argument] if the
        value has not the valid type *)
end

(**/**)

open Wire

module type Reader = sig
  val read_variant : t reader
  val read : Types.t list -> t list reader
end

module type Writer = sig
  val write_variant : t writer
  val write : t list writer
end

module LEReader : Reader
module BEReader : Reader
module LEWriter : Writer
module BEWriter : Writer
