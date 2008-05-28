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

let fake padding = typ ("$" ^ string_of_int padding ^ "value") []
let fakes padding = typ ("$" ^ string_of_int padding ^ "values") []
let fake1 = fake 1
let fake8 = fake 8
let fakes1 = fakes 1
let fakes8 = fakes 8

let rules = default_rules @ intern_rules

let compr code env tenv ret =
  GenCode.generate_reader false false (Env.Type.add tenv (Env.init env))
    (optimize false 0 1 code).opt_code
    (fun _ -> <:expr< $ret$ >>)

let compw code env tenv ret =
  GenCode.generate_writer false false (Env.Type.add tenv (Env.init env))
    (optimize false 0 1 code).opt_code
    (fun _ -> <:expr< $ret$ >>)

let genr caml_type dbus_type name tenv =
  compr (snd (gen_reader trace rules caml_type [dbus_type] [])) 0 tenv
    (<:expr< (i, $uid:name$ v0) >>)

let genw caml_type dbus_type tenv =
  compw (snd (gen_writer trace rules caml_type [dbus_type] [])) 1 tenv
    (<:expr< (buffer, i) >>)

let rarray1_env, rarray1_code = gen_reader trace rules (list fake1) [Tarray Tbyte] []
let warray1_env, warray1_code = gen_writer trace rules (list fake1) [Tarray Tbyte] []
let rarray8_env, rarray8_code = gen_reader trace rules (list fake8) [Tarray Tbyte] rarray1_env
let warray8_env, warray8_code = gen_writer trace rules (list fake8) [Tarray Tbyte] warray1_env
let rdict_env, rdict_code = gen_reader trace rules (list (tuple[fake8; fake1])) [Tdict(Tbyte, Tbyte)] rarray8_env
let wdict_env, wdict_code = gen_writer trace rules (list (tuple[fake8; fake1])) [Tdict(Tbyte, Tbyte)] warray8_env

let (rarray1_id, rarray1_expr) = List.hd rarray1_env
let (warray1_id, warray1_expr) = List.hd warray1_env
let (rarray8_id, rarray8_expr) = List.hd rarray8_env
let (warray8_id, warray8_expr) = List.hd warray8_env
let (rdict_id, rdict_expr) = List.hd rdict_env
let (wdict_id, wdict_expr) = List.hd wdict_env

let implem bo =
  (<:str_item<
   module $uid:bo ^ "Reader"$ =
   struct
     open Wire.$uid:bo ^ "Reader"$

     let rec $id:rarray1_id$ = $rarray1_expr$
     and $id:rarray8_id$ = $rarray1_expr$
     and $id:rdict_id$ = $rdict_expr$
     and read_value buffer i = function
       | Tbyte -> $genr char Tbyte "Byte" 0$
       | Tboolean -> $genr bool Tboolean "Boolean" 0$
       | Tint16 -> $genr int Tint16 "Int16" 0$
       | Tint32 -> $genr int32 Tint32 "Int32" 0$
       | Tint64 -> $genr int64 Tint64 "Int64" 0$
       | Tuint16 -> $genr int Tuint16 "Uint16" 0$
       | Tuint32 -> $genr int32 Tuint32 "Uint32" 0$
       | Tuint64 -> $genr int64 Tuint64 "Uint64" 0$
       | Tdouble -> $genr float Tdouble "Double" 0$
       | Tstring -> $genr string Tstring "String" 0$
       | Tsignature -> $genr obus_dtypes Tsignature "Signature" 0$
       | Tobject_path -> $genr string Tobject_path "Object_path" 0$
       | Tarray(typ0) -> begin match typ0 with
           | Tint64
           | Tuint64
           | Tdouble
           | Tstructure _ -> $compr rarray8_code 0 1 (<:expr< (i, Array(typ0, v0)) >>)$
           | _ -> $compr rarray1_code 0 1 (<:expr< (i, Array(typ0, v0)) >>)$
         end
       | Tdict(typ0, typ1) -> $compr rdict_code 0 2 (<:expr< (i, Dict(typ0, typ1, v0)) >>)$
       | Tstructure(typ0) -> $genr fakes8 Tvariant "Structure" 1$
       | Tvariant -> $genr obus_value Tvariant "Variant" 0$
     and read_values buffer i = function
       | t :: ts ->
           let (i, v) = read_value buffer i t in
           let (i, vs) = read_values buffer i ts in
             (i, v :: vs)
       | [] -> (i, [])
   end

   module $uid:bo ^ "Writer"$ =
   struct
     open Wire.$uid:bo ^ "Writer"$

     let rec $id:warray1_id$ = $warray1_expr$
     and $id:warray8_id$ = $warray1_expr$
     and $id:wdict_id$ = $wdict_expr$
     and write_value buffer i = function
       | Byte(v0) -> $genw char Tbyte 0$
       | Boolean(v0) -> $genw bool Tboolean 0$
       | Int16(v0) -> $genw int Tint16 0$
       | Int32(v0) -> $genw int32 Tint32 0$
       | Int64(v0) -> $genw int64 Tint64 0$
       | Uint16(v0) -> $genw int Tuint16 0$
       | Uint32(v0) -> $genw int32 Tuint32 0$
       | Uint64(v0) -> $genw int64 Tuint64 0$
       | Double(v0) -> $genw float Tdouble 0$
       | String(v0) -> $genw string Tstring 0$
       | Signature(v0) -> $genw obus_dtypes Tsignature 0$
       | Object_path(v0) -> $genw string Tobject_path 0$
       | Array(t, v0) -> begin match t with
           | Tint64
           | Tuint64
           | Tdouble
           | Tstructure _ -> $compw warray8_code 1 0 (<:expr< (buffer, i) >>)$
           | _ -> $compw warray1_code 1 0 (<:expr< (buffer, i) >>)$
         end
       | Dict(_, _, v0) -> $compw wdict_code 1 0 (<:expr< (buffer, i) >>)$
       | Structure(v0) -> $genw fakes8 Tvariant 1$
       | Variant(v0) -> $genw obus_value Tvariant 0$
     and write_values buffer i vs = List.fold_left (fun (buffer, i) v -> write_value buffer i v) (buffer, i) vs
   end
     >>)

let _ =
  Printers.OCaml.print_implem
    (<:str_item<
     open Wire
     $implem "LE"$;;
     $implem "BE"$
     >>)
