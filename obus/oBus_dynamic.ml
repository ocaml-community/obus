(*
 * oBus_dynamic.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_types

type 'a unsafe = 'a * Obj.t
type 'a t = 'a unsafe
type variant = single t
type body = sequence t

let of_unsafe x = x
let to_unsafe x = x

type ('a, 'cl) ty = 'cl

let make t v = (t, Obj.repr v)
let get t (t', v) = if t = t' then Obj.obj v else failwith "OBus_dynamic.get: invalid type"

let typ = fst
let dynamic_ty t = t

type ('cl, 'b) with_ty = { with_ty : 'a. ('a, 'cl) ty -> 'b }
let with_ty { with_ty = f } = f

type ('cl, 'b) with_content = { with_content : 'a. ('a, 'cl) ty -> 'a -> 'b }
let with_content { with_content = f } (t, v) = f t v

let with_basic (matcher : < match_basic : 'a. ('a, basic) ty -> 'a -> 'b; .. >) (t, v) = matcher#match_basic t v
let with_single (matcher : < match_single : 'a. ('a, single) ty -> 'a -> 'b; .. >) (t, v) = matcher#match_single t v
let with_sequence (matcher : < match_sequence : 'a. ('a, sequence) ty -> 'a -> 'b; .. >) (t, v) = matcher#match_sequence t v

let with_basic_ty matcher t = matcher#match_basic t
let with_single_ty matcher t = matcher#match_single t
let with_sequence_ty matcher t = matcher#match_sequence t

let tbyte = Tbyte
let tboolean = Tboolean
let tint16 = Tint16
let tint32 = Tint32
let tint64 = Tint64
let tuint16 = Tint16
let tuint32 = Tuint32
let tuint64 = Tuint64
let tdouble = Tdouble
let tstring = Tstring
let tsignature = Tsignature
let tobject_path = Tobject_path
let tbasic t = Tbasic t
let tarray t = Tarray t
let tdict tk tv = Tdict(tk, tv)
let tstruct tl = Tstruct tl
let tvariant = Tvariant
let tcons : single -> sequence -> sequence = fun ta tb -> ta :: tb
let tnil = []

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

module Matcher(Branch : sig type ('a, 'cl) t end) =
struct
  type ('a, 'cl) branch = ('a, 'cl) Branch.t

  class matcher = object(self)
    method default_basic : 'a. ('a, basic) ty -> ('a, basic) branch = fun t -> failwith ("OBus_dynamic.Matcher.matcher#match_basic: match failure")
    method default_single : 'a. ('a, single) ty -> ('a, single) branch = fun t -> failwith ("OBus_dynamic.Matcher.matcher#match_single: match failure")
    method default_sequence : 'a. ('a, sequence) ty -> ('a, sequence) branch = fun t -> failwith ("OBus_dynamic.Matcher.matcher#match_sequence: match failure")

    method byte : (char, basic) branch = self#default_basic tbyte
    method boolean : (bool, basic) branch = self#default_basic tboolean
    method int16 : (int, basic) branch = self#default_basic tint16
    method int32 : (int32, basic) branch = self#default_basic tint32
    method int64 : (int64, basic) branch = self#default_basic tint64
    method uint16 : (int, basic) branch = self#default_basic tuint16
    method uint32 : (int32, basic) branch = self#default_basic tuint32
    method uint64 : (int64, basic) branch = self#default_basic tuint64
    method double : (float, basic) branch = self#default_basic tdouble
    method string : (string, basic) branch = self#default_basic tstring
    method signature : (signature, basic) branch = self#default_basic tsignature
    method object_path : (OBus_path.t, basic) branch = self#default_basic tobject_path

    method basic : 'a. ('a, basic) ty -> ('a, single) branch = fun t -> self#default_single (tbasic t)
    method array : 'a. ('a, single) ty -> ('a list, single) branch = fun t -> self#default_single (tarray t)
    method dict : 'a 'b. ('a, basic) ty -> ('b, single) ty -> (('a * 'b) list, single) branch = fun tk tv -> self#default_single (tdict tk tv)
    method structure : 'a. ('a, sequence) ty -> ('a, single) branch = fun tl -> self#default_single (tstruct tl)
    method variant : (variant, single) branch = self#default_single tvariant

    method cons : 'a 'b. ('a, single) ty -> ('b, sequence) ty -> ('a * 'b, sequence) branch = fun ta tb -> self#default_sequence (tcons ta tb)
    method nil : (unit, sequence) branch = self#default_sequence tnil

    method match_basic : 'a. ('a, basic) ty -> ('a, basic) branch = function
      | Tbyte -> Obj.magic (self#byte)
      | Tboolean -> Obj.magic (self#boolean)
      | Tint16 -> Obj.magic (self#int16)
      | Tint32 -> Obj.magic (self#int32)
      | Tint64 -> Obj.magic (self#int64)
      | Tuint16 -> Obj.magic (self#uint16)
      | Tuint32 -> Obj.magic (self#uint32)
      | Tuint64 -> Obj.magic (self#uint64)
      | Tdouble -> Obj.magic (self#double)
      | Tstring -> Obj.magic (self#string)
      | Tsignature -> Obj.magic (self#signature)
      | Tobject_path -> Obj.magic (self#object_path)

    method match_single : 'a. ('a, single) ty -> ('a, single) branch = function
      | Tbasic t -> Obj.magic (self#basic t)
      | Tarray t -> Obj.magic (self#array t)
      | Tdict(tk, tv) -> Obj.magic (self#dict tk tv)
      | Tstruct tl -> Obj.magic (self#structure tl)
      | Tvariant -> Obj.magic (self#variant)

    method match_sequence : 'a. ('a, sequence) ty -> ('a, sequence) branch = function
      | hd :: tl -> Obj.magic (self#cons hd tl)
      | [] -> Obj.magic (self#nil)
  end
end

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

module Maker(Combinators : Combinators) =
struct
  open Combinators

  let make_basic : ('a, basic) ty -> ('a, basic) Combinators.t = function
    | Tbyte -> Obj.magic cbyte
    | Tboolean -> Obj.magic cboolean
    | Tint16 -> Obj.magic cint16
    | Tint32 -> Obj.magic cint32
    | Tint64 -> Obj.magic cint64
    | Tuint16 -> Obj.magic cuint16
    | Tuint32 -> Obj.magic cuint32
    | Tuint64 -> Obj.magic cuint64
    | Tdouble -> Obj.magic cdouble
    | Tstring -> Obj.magic cstring
    | Tsignature -> Obj.magic csignature
    | Tobject_path -> Obj.magic cobject_path

  let rec make_single : ('a, single) ty -> ('a, single) Combinators.t = function
    | Tbasic t -> Obj.magic (cbasic (make_basic t))
    | Tstruct tl -> Obj.magic (cstruct (make_sequence tl))
    | Tarray t -> Obj.magic (carray (make_single t))
    | Tdict(tk, tv) -> Obj.magic (cdict (make_basic tk) (make_single tv))
    | Tvariant -> Obj.magic cvariant

  and make_sequence : ('a, sequence) ty -> ('a, sequence) Combinators.t = function
    | t :: tl -> Obj.magic (ccons (make_single t) (make_sequence tl))
    | [] -> Obj.magic cnil

  class make = object
    method match_basic : 'a. ('a, basic) ty -> ('a, basic) Combinators.t = make_basic
    method match_single : 'a. ('a, single) ty -> ('a, single) Combinators.t = make_single
    method match_sequence : 'a. ('a, sequence) ty -> ('a, sequence) Combinators.t = make_sequence
  end
end

module Printers =
struct
  open Printf
  type ('a, 'cl) t = 'a -> string
  let cbyte = sprintf "%C"
  let cboolean = string_of_bool
  let cint16 = string_of_int
  let cint32 = sprintf "%ldl"
  let cint64 = sprintf "%Ldl"
  let cuint16 = string_of_int
  let cuint32 = sprintf "%ldl"
  let cuint64 = sprintf "%Ldl"
  let cdouble = string_of_float
  let cstring = sprintf "%S"
  let csignature s = sprintf "S<%s>" (OBus_types.string_of_sequence s)
  let cobject_path = sprintf "P<%S>"
  let cbasic f = f
  let carray f l = sprintf "[%s]" (String.concat "; " (List.map f l))
  let cdict fk fv l =
    sprintf "[%s]"
      (String.concat "; " (List.map (fun (a, b) -> sprintf "(%s, %s)" (fk a) (fv b)) l))
  let cstruct f x = sprintf "(%s)" (f x)
  let cvariant f (t, v) = sprintf "V<%s : %s>" (f v) (OBus_types.string_of_single (dynamic_ty t))
  let ccons fa fb (a, b) = match fb b with
    | "" -> fa a
    | s -> sprintf "%s * %s" (fa a) s
  let cnil () = ""
end

module P = Maker(Printers)

let string_of_basic (t, v) = P.make_basic t v
let string_of_single (t, v) = P.make_single t v
let string_of_sequence (t, v) = P.make_sequence t v
