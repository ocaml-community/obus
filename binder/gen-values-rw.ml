(*
 * pa_gen_values_rw.ml
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Generate readers/writers for general dbus values *)

open Camlp4.PreCast
open Types
open GenSerializer
open Optimize
open Helpers

let _loc = Loc.ghost

let trace = false

let fake = typ "$value" []

let rules = default_rules @ intern_rules

let compr code env tenv ret =
  GenCode.generate_reader false (Env.Type.add tenv (Env.init env))
    (optimize false 0 1 code).opt_code
    (fun _ -> <:expr< $ret$ >>)

let compw code env tenv ret =
  GenCode.generate_writer false (Env.Type.add tenv (Env.init env))
    (optimize false 0 1 code).opt_code
    (fun _ -> <:expr< $ret$ >>)

let genr caml_type dbus_type name =
  compr (snd (gen_reader trace rules caml_type [dbus_type] [])) 0 0
    (<:expr< (i, $uid:name$ v0) >>)

let genw caml_type dbus_type =
  compw (snd (gen_writer trace rules caml_type [dbus_type] [])) 1 0
    (<:expr< (buffer, i) >>)

let rarray_env, rarray_code = gen_reader trace rules (list fake) [Tarray Tbyte] []
let warray_env, warray_code = gen_writer trace rules (list fake) [Tarray Tbyte] []
let rdict_env, rdict_code = gen_reader trace rules (list (tuple[fake; fake])) [Tdict(Tbyte, Tbyte)] rarray_env
let wdict_env, wdict_code = gen_writer trace rules (list (tuple[fake; fake])) [Tdict(Tbyte, Tbyte)] warray_env

let (rarray_id, rarray_expr) = List.hd rarray_env
let (warray_id, warray_expr) = List.hd warray_env
let (rdict_id, rdict_expr) = List.hd rdict_env
let (wdict_id, wdict_expr) = List.hd wdict_env

let implem bo =
  (<:str_item<
   module $uid:bo ^ "Reader"$ =
   struct
     open Wire.$uid:bo ^ "Reader"$

     let rec $id:rarray_id$ = fun typ_1 -> $rarray_expr$
     and $id:rdict_id$ = fun typ_1 typ_2 -> $rdict_expr$
     and read_value buffer i = function
       | Tbyte -> $genr char Tbyte "Byte"$
       | Tboolean -> $genr bool Tboolean "Boolean"$
       | Tint16 -> $genr int Tint16 "Int16"$
       | Tint32 -> $genr int32 Tint32 "Int32"$
       | Tint64 -> $genr int64 Tint64 "Int64"$
       | Tuint16 -> $genr int Tuint16 "Uint16"$
       | Tuint32 -> $genr int32 Tuint32 "Uint32"$
       | Tuint64 -> $genr int64 Tuint64 "Uint64"$
       | Tdouble -> $genr float Tdouble "Double"$
       | Tstring -> $genr string Tstring "String"$
       | Tsignature -> $genr obus_dtypes Tsignature "Signature"$
       | Tobject_path -> $genr string Tobject_path "Object_path"$
       | Tarray(typ0) -> $compr rarray_code 0 1 (<:expr< (i, Array(typ0, v0)) >>)$
       | Tdict(typ0, typ1) -> $compr rarray_code 0 2 (<:expr< (i, Dict(typ0, typ1, v0)) >>)$
       | Tvariant -> $genr obus_value Tvariant "Variant"$
   end

   module $uid:bo ^ "Writer"$ =
   struct
     open Wire.$uid:String.uppercase bo ^ "Writer"$

     let rec $id:warray_id$ = $warray_expr$
     and $id:wdict_id$ = $wdict_expr$
     and write_value buffer i = function
       | Byte(v0) -> $genw char Tbyte$
       | Boolean(v0) -> $genw bool Tboolean$
       | Int16(v0) -> $genw int Tint16$
       | Int32(v0) -> $genw int32 Tint32$
       | Int64(v0) -> $genw int64 Tint64$
       | Uint16(v0) -> $genw int Tuint16$
       | Uint32(v0) -> $genw int32 Tuint32$
       | Uint64(v0) -> $genw int64 Tuint64$
       | Double(v0) -> $genw float Tdouble$
       | String(v0) -> $genw string Tstring$
       | Signature(v0) -> $genw obus_dtypes Tsignature$
       | Object_path(v0) -> $genw string Tobject_path$
       | Array(_, v0) -> $compw warray_code 1 0 (<:expr< (buffer, i) >>)$
       | Dict(_, _, v0) -> $compw warray_code 1 0 (<:expr< (buffer, i) >>)$
       | Variant(v0) -> $genw obus_value Tvariant$
   end
     >>)

let _ =
  Printers.OCaml.print_implem
    (<:str_item<
     open Wire
     open Internal
     $implem "LE"$;;
     $implem "BE"$
     >>)
