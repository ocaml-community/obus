(*
 * OBusTypes.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Representation of DBus types *)

module type S = sig
  type basic =
      private
    | Byte
    | Boolean
    | Int16
    | Int32
    | Int64
    | Uint16
    | Uint32
    | Uint64
    | Double
    | String
    | Signature
    | Object_path

  type single =
      private
    | Basic of basic
    | Array of single
    | Dict of basic * single
    | Structure of single list
    | Variant

  type t = single list

  val string_of_basic : basic -> string
  val string_of_single : single -> string
  val string_of_t : t -> string

  type byte
  type boolean
  type int16
  type int32
  type int64
  type uint16
  type uint32
  type uint64
  type double
  type string
  type signature
  type object_path
  type 'a array
  type ('a, 'b) dict
  type 'a structure
  type variant
  type nil

  type 'a tbasic
  type 'a tsingle
  type ('a, 'b) tlist

  val byte : byte tbasic
  val boolean : boolean tbasic
  val int16 : int16 tbasic
  val int32 : int32 tbasic
  val int64 : int64 tbasic
  val uint16 : uint16 tbasic
  val uint32 : uint32 tbasic
  val uint64 : uint64 tbasic
  val double : double tbasic
  val string : string tbasic
  val signature : signature tbasic
  val object_path : object_path tbasic
  val basic : 'a tbasic -> 'a tsingle
  val array : 'a tsingle -> 'a array tsingle
  val dict : 'a tbasic -> 'b tsingle -> ('a, 'b) dict tsingle
  val structure : ('a, nil) tlist -> 'a structure tsingle
  val variant : variant tsingle
  val cons : 'a tsingle -> ('b, 'c) tlist -> ('a * 'b, 'c) tlist
  val nil : ('a, 'a) tlist
  val concat : ('a, 'b) tlist -> ('b, 'c) tlist -> ('a, 'c) tlist

  val basic_of_tbasic : 'a tbasic -> basic
  val single_of_tsingle : 'a tsingle -> single
  val list_of_tlist : ('a, 'b) tlist -> t
end
