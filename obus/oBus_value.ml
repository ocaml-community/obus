(*
 * oBus_value.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_types

type basic =
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
  | Signature of OBus_types.signature
  | Object_path of OBus_path.t

type single =
  | Basic of basic
  | Array of OBus_types.single * single list
  | Dict of OBus_types.basic * OBus_types.single * (basic * single) list
  | Struct of single list
  | Variant of single

type sequence = single list

let type_of_basic = function
  | Byte _ -> Tbyte
  | Boolean _ -> Tboolean
  | Int16 _ -> Tint16
  | Int32 _ -> Tint32
  | Int64 _ -> Tint64
  | Uint16 _ -> Tuint16
  | Uint32 _ -> Tuint32
  | Uint64 _ -> Tuint64
  | Double _ -> Tdouble
  | String _ -> Tstring
  | Signature _ -> Tsignature
  | Object_path _ -> Tobject_path

let rec type_of_single = function
  | Basic x -> Tbasic(type_of_basic x)
  | Array(t, x) -> Tarray t
  | Dict(tk, tv, x) -> Tdict(tk, tv)
  | Struct x -> Tstruct(List.map type_of_single x)
  | Variant _ -> Tvariant

let type_of_sequence = List.map type_of_single

let vbyte x = Byte x
let vboolean x = Boolean x
let vint16 x = Int16 x
let vint32 x = Int32 x
let vint64 x = Int64 x
let vuint16 x = Uint16 x
let vuint32 x = Uint32 x
let vuint64 x = Uint64 x
let vdouble x = Double x
let vstring x = String x
let vsignature x = Signature x
let vobject_path x = Object_path x
let vbasic x = Basic x
let varray t l =
  List.iter (fun x ->
               if type_of_single x <> t
               then failwith "OBus_value.varray: unexpected type") l;
  Array(t, l)
let vdict tk tv l =
  List.iter (fun (k, v) ->
               if type_of_basic k <> tk or type_of_single v <> tv
               then failwith "OBus_value.vdict: unexpected type") l;
  Dict(tk, tv, l)
let vstruct l = Struct l
let vvariant v = Variant v

exception Cast_failure

class type ['a] ty_sequence = object
  method make_sequence : 'a -> sequence
  method cast_sequence : sequence -> 'a
  method type_sequence : OBus_types.sequence
end

class virtual ['a] ty_single = object(self)
  method virtual make_single : 'a -> single
  method virtual cast_single : single -> 'a
  method virtual type_single : OBus_types.single
  method make_sequence x = [self#make_single x]
  method cast_sequence = function
    | [x] -> self#cast_single x
    | _ -> raise Cast_failure
  method type_sequence = [self#type_single]
end

class virtual ['a] ty_basic = object(self)
  inherit ['a] ty_single
  method virtual make_basic : 'a -> basic
  method virtual cast_basic : basic -> 'a
  method virtual type_basic : OBus_types.basic
  method make_single x = Basic(self#make_basic x)
  method cast_single = function
    | Basic x -> self#cast_basic x
    | _ -> raise Cast_failure
  method type_single = Tbasic(self#type_basic)
end

let make_basic ty = ty#make_basic
let make_single ty = ty#make_single
let make_sequence ty = ty#make_sequence

let cast_basic ty = ty#cast_basic
let cast_single ty = ty#cast_single
let cast_sequence ty = ty#cast_sequence

let opt_cast cast ty v =
  try
    Some(cast ty v)
  with
    | Cast_failure -> None

let opt_cast_basic ty = opt_cast cast_basic ty
let opt_cast_single ty = opt_cast cast_single ty
let opt_cast_sequence ty = opt_cast cast_sequence ty

let type_of_basic_ty ty = ty#type_basic
let type_of_single_ty ty = ty#type_single
let type_of_sequence_ty ty = ty#type_sequence

let tbyte = object
  inherit [char] ty_basic
  method make_basic x = Byte x
  method cast_basic = function
    | Byte x -> x
    | _ -> raise Cast_failure
  method type_basic = Tbyte
end

let tboolean = object
  inherit [bool] ty_basic
  method make_basic x = Boolean x
  method cast_basic = function
    | Boolean x -> x
    | _ -> raise Cast_failure
  method type_basic = Tboolean
end

let tint16 = object
  inherit [int] ty_basic
  method make_basic x = Int16 x
  method cast_basic = function
    | Int16 x -> x
    | _ -> raise Cast_failure
  method type_basic = Tint16
end

let tint32 = object
  inherit [int32] ty_basic
  method make_basic x = Int32 x
  method cast_basic = function
    | Int32 x -> x
    | _ -> raise Cast_failure
  method type_basic = Tint32
end

let tint64 = object
  inherit [int64] ty_basic
  method make_basic x = Int64 x
  method cast_basic = function
    | Int64 x -> x
    | _ -> raise Cast_failure
  method type_basic = Tint64
end

let tuint16 = object
  inherit [int] ty_basic
  method make_basic x = Uint16 x
  method cast_basic = function
    | Uint16 x -> x
    | _ -> raise Cast_failure
  method type_basic = Tuint16
end

let tuint32 = object
  inherit [int32] ty_basic
  method make_basic x = Uint32 x
  method cast_basic = function
    | Uint32 x -> x
    | _ -> raise Cast_failure
  method type_basic = Tuint32
end

let tuint64 = object
  inherit [int64] ty_basic
  method make_basic x = Uint64 x
  method cast_basic = function
    | Uint64 x -> x
    | _ -> raise Cast_failure
  method type_basic = Tuint64
end

let tdouble = object
  inherit [float] ty_basic
  method make_basic x = Double x
  method cast_basic = function
    | Double x -> x
    | _ -> raise Cast_failure
  method type_basic = Tdouble
end

let tstring = object
  inherit [string] ty_basic
  method make_basic x = String x
  method cast_basic = function
    | String x -> x
    | _ -> raise Cast_failure
  method type_basic = Tstring
end

let tsignature = object
  inherit [signature] ty_basic
  method make_basic x = Signature x
  method cast_basic = function
    | Signature x -> x
    | _ -> raise Cast_failure
  method type_basic = Tsignature
end

let tobject_path = object
  inherit [OBus_path.t] ty_basic
  method make_basic x = Object_path x
  method cast_basic = function
    | Object_path x -> x
    | _ -> raise Cast_failure
  method type_basic = Tobject_path
end

let tarray (ty : 'a #ty_single) = object
  inherit ['a list] ty_single
  method make_single l = Array(type_of_single_ty ty, List.map (make_single ty) l)
  method cast_single = function
    | Array(t, l) when t = type_of_single_ty ty ->
        List.map (cast_single ty) l
    | _ -> raise Cast_failure
  method type_single = Tarray(type_of_single_ty ty)
end

let tdict (tyk : 'a #ty_basic) (tyv : 'b #ty_single) = object
  inherit [('a * 'b) list] ty_single
  method make_single l = Dict(type_of_basic_ty tyk,
                              type_of_single_ty tyv,
                              List.map (fun (k, v) -> (make_basic tyk k,
                                                       make_single tyv v)) l)
  method cast_single = function
    | Dict(tk, tv, l) when tk = type_of_basic_ty tyk && tv = type_of_single_ty tyv ->
        List.map (fun (k, v) -> (cast_basic tyk k, cast_single tyv v)) l
    | _ -> raise Cast_failure
  method type_single = Tdict(type_of_basic_ty tyk, type_of_single_ty tyv)
end

let tstruct (ty : 'a #ty_sequence) = object
  inherit ['a] ty_single
  method make_single l = Struct (make_sequence ty l)
  method cast_single =function
    | Struct l -> cast_sequence ty l
    | _ -> raise Cast_failure
  method type_single = Tstruct (type_of_sequence_ty ty)
end

let tvariant = object
  inherit [single] ty_single
  method make_single x = Variant x
  method cast_single = function
    | Variant v -> v
    | _ -> raise Cast_failure
  method type_single = Tvariant
end

let tnil = object
  method make_sequence () = []
  method cast_sequence = function
    | [] -> ()
    | _ -> raise Cast_failure
  method type_sequence = []
end

let tcons tyx tyl = object
  method make_sequence (x, l) = make_single tyx x :: make_sequence tyl l
  method cast_sequence = function
    | x :: l ->
        let a = cast_single tyx x
        and b = cast_sequence tyl l in
          (a, b)
    | [] -> raise Cast_failure
  method type_sequence = type_of_single_ty tyx :: type_of_sequence_ty tyl
end

let tup0 = tnil
let tup1 (ty1 : 'a #ty_single) = (ty1 :> 'a ty_sequence)
let tup2 ty1 ty2 = object
  method make_sequence (x1, x2) =
    [make_single ty1 x1;
     make_single ty2 x2]
  method cast_sequence = function
    | [x1; x2] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2 in
          (v1, v2)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2]
end
let tup3 ty1 ty2 ty3 = object
  method make_sequence (x1, x2, x3) =
    [make_single ty1 x1;
     make_single ty2 x2;
     make_single ty3 x3]
  method cast_sequence = function
    | [x1; x2; x3] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2
        and v3 = cast_single ty3 x3 in
          (v1, v2, v3)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2;
                          type_of_single_ty ty3]
end
let tup4 ty1 ty2 ty3 ty4 = object
  method make_sequence (x1, x2, x3, x4) =
    [make_single ty1 x1;
     make_single ty2 x2;
     make_single ty3 x3;
     make_single ty4 x4]
  method cast_sequence =function
    | [x1; x2; x3; x4] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2
        and v3 = cast_single ty3 x3
        and v4 = cast_single ty4 x4 in
          (v1, v2, v3, v4)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2;
                          type_of_single_ty ty3;
                          type_of_single_ty ty4]
end
let tup5 ty1 ty2 ty3 ty4 ty5 = object
  method make_sequence (x1, x2, x3, x4, x5) =
    [make_single ty1 x1;
     make_single ty2 x2;
     make_single ty3 x3;
     make_single ty4 x4;
     make_single ty5 x5]
  method cast_sequence = function
    | [x1; x2; x3; x4; x5] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2
        and v3 = cast_single ty3 x3
        and v4 = cast_single ty4 x4
        and v5 = cast_single ty5 x5 in
          (v1, v2, v3, v4, v5)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2;
                          type_of_single_ty ty3;
                          type_of_single_ty ty4;
                          type_of_single_ty ty5]
end
let tup6 ty1 ty2 ty3 ty4 ty5 ty6 = object
  method make_sequence (x1, x2, x3, x4, x5, x6) =
    [make_single ty1 x1;
     make_single ty2 x2;
     make_single ty3 x3;
     make_single ty4 x4;
     make_single ty5 x5;
     make_single ty6 x6]
  method cast_sequence = function
    | [x1; x2; x3; x4; x5; x6] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2
        and v3 = cast_single ty3 x3
        and v4 = cast_single ty4 x4
        and v5 = cast_single ty5 x5
        and v6 = cast_single ty6 x6 in
          (v1, v2, v3, v4, v5, v6)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2;
                          type_of_single_ty ty3;
                          type_of_single_ty ty4;
                          type_of_single_ty ty5;
                          type_of_single_ty ty6]
end
let tup7 ty1 ty2 ty3 ty4 ty5 ty6 ty7 = object
  method make_sequence (x1, x2, x3, x4, x5, x6, x7) =
    [make_single ty1 x1;
     make_single ty2 x2;
     make_single ty3 x3;
     make_single ty4 x4;
     make_single ty5 x5;
     make_single ty6 x6;
     make_single ty7 x7]
  method cast_sequence = function
    | [x1; x2; x3; x4; x5; x6; x7] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2
        and v3 = cast_single ty3 x3
        and v4 = cast_single ty4 x4
        and v5 = cast_single ty5 x5
        and v6 = cast_single ty6 x6
        and v7 = cast_single ty7 x7 in
          (v1, v2, v3, v4, v5, v6, v7)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2;
                          type_of_single_ty ty3;
                          type_of_single_ty ty4;
                          type_of_single_ty ty5;
                          type_of_single_ty ty6;
                          type_of_single_ty ty7]
end
let tup8 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 = object
  method make_sequence (x1, x2, x3, x4, x5, x6, x7, x8) =
    [make_single ty1 x1;
     make_single ty2 x2;
     make_single ty3 x3;
     make_single ty4 x4;
     make_single ty5 x5;
     make_single ty6 x6;
     make_single ty7 x7;
     make_single ty8 x8]
  method cast_sequence = function
    | [x1; x2; x3; x4; x5; x6; x7; x8] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2
        and v3 = cast_single ty3 x3
        and v4 = cast_single ty4 x4
        and v5 = cast_single ty5 x5
        and v6 = cast_single ty6 x6
        and v7 = cast_single ty7 x7
        and v8 = cast_single ty8 x8 in
          (v1, v2, v3, v4, v5, v6, v7, v8)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2;
                          type_of_single_ty ty3;
                          type_of_single_ty ty4;
                          type_of_single_ty ty5;
                          type_of_single_ty ty6;
                          type_of_single_ty ty7;
                          type_of_single_ty ty8]
end
let tup9 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 ty9 = object
  method make_sequence (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    [make_single ty1 x1;
     make_single ty2 x2;
     make_single ty3 x3;
     make_single ty4 x4;
     make_single ty5 x5;
     make_single ty6 x6;
     make_single ty7 x7;
     make_single ty8 x8;
     make_single ty9 x9]
  method cast_sequence = function
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2
        and v3 = cast_single ty3 x3
        and v4 = cast_single ty4 x4
        and v5 = cast_single ty5 x5
        and v6 = cast_single ty6 x6
        and v7 = cast_single ty7 x7
        and v8 = cast_single ty8 x8
        and v9 = cast_single ty9 x9 in
          (v1, v2, v3, v4, v5, v6, v7, v8, v9)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2;
                          type_of_single_ty ty3;
                          type_of_single_ty ty4;
                          type_of_single_ty ty5;
                          type_of_single_ty ty6;
                          type_of_single_ty ty7;
                          type_of_single_ty ty8;
                          type_of_single_ty ty9]
end
let tup10 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 ty9 ty10 = object
  method make_sequence (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =
    [make_single ty1 x1;
     make_single ty2 x2;
     make_single ty3 x3;
     make_single ty4 x4;
     make_single ty5 x5;
     make_single ty6 x6;
     make_single ty7 x7;
     make_single ty8 x8;
     make_single ty9 x9;
     make_single ty10 x10]
  method cast_sequence = function
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10] ->
        let v1 = cast_single ty1 x1
        and v2 = cast_single ty2 x2
        and v3 = cast_single ty3 x3
        and v4 = cast_single ty4 x4
        and v5 = cast_single ty5 x5
        and v6 = cast_single ty6 x6
        and v7 = cast_single ty7 x7
        and v8 = cast_single ty8 x8
        and v9 = cast_single ty9 x9
        and v10 = cast_single ty10 x10 in
          (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
    | _ -> raise Cast_failure
  method type_sequence = [type_of_single_ty ty1;
                          type_of_single_ty ty2;
                          type_of_single_ty ty3;
                          type_of_single_ty ty4;
                          type_of_single_ty ty5;
                          type_of_single_ty ty6;
                          type_of_single_ty ty7;
                          type_of_single_ty ty8;
                          type_of_single_ty ty9;
                          type_of_single_ty ty10]
end

type 'a with_basic_ty = { with_basic_ty : 'b. 'b ty_basic -> 'a }
type 'a with_single_ty = { with_single_ty : 'b. 'b ty_single -> 'a }
type 'a with_sequence_ty = { with_sequence_ty : 'b. 'b ty_sequence -> 'a }

let with_basic_ty w = function
  | Tbyte -> w.with_basic_ty tbyte
  | Tboolean -> w.with_basic_ty tboolean
  | Tint16 -> w.with_basic_ty tint16
  | Tint32 -> w.with_basic_ty tint32
  | Tint64 -> w.with_basic_ty tint64
  | Tuint16 -> w.with_basic_ty tuint16
  | Tuint32 -> w.with_basic_ty tuint32
  | Tuint64 -> w.with_basic_ty tuint64
  | Tdouble -> w.with_basic_ty tdouble
  | Tstring -> w.with_basic_ty tstring
  | Tsignature -> w.with_basic_ty tsignature
  | Tobject_path -> w.with_basic_ty tobject_path

let rec with_single_ty w = function
  | Tbasic t -> with_basic_ty { with_basic_ty = fun t -> w.with_single_ty (t :> _ ty_single) } t
  | Tarray t -> with_single_ty { with_single_ty = fun t -> w.with_single_ty (tarray t) } t
  | Tdict(tk, tv) -> with_basic_ty { with_basic_ty = fun tk ->
                                       with_single_ty { with_single_ty = fun tv -> w.with_single_ty (tdict tk tv) } tv } tk
  | Tstruct tl -> with_sequence_ty { with_sequence_ty = fun t -> w.with_single_ty (tstruct t) } tl
  | Tvariant -> w.with_single_ty tvariant

and with_sequence_ty w = function
  | [] -> w.with_sequence_ty tnil
  | tx :: tl -> with_single_ty { with_single_ty = fun tx ->
                                   with_sequence_ty { with_sequence_ty = fun tl -> w.with_sequence_ty (tcons tx tl) } tl } tx

open Printf

let string_of_basic = function
  | Byte x -> sprintf "Byte %C" x
  | Boolean x -> sprintf "Boolean %B" x
  | Int16 x -> sprintf "Int16 %d" x
  | Int32 x -> sprintf "Int32 %ldl" x
  | Int64 x -> sprintf "Int64 %LdL" x
  | Uint16 x -> sprintf "Uint16 %d" x
  | Uint32 x -> sprintf "Uint32 %ldl" x
  | Uint64 x -> sprintf "Uint64 %LdL" x
  | Double x -> sprintf "Double %f" x
  | String x -> sprintf "String %S" x
  | Signature x -> sprintf "Signature(%s)" (OBus_types.string_of_sequence x)
  | Object_path x -> sprintf "Object_path %S" x

let rec string_of_single = function
  | Basic v -> sprintf "Basic(%s)" (string_of_basic v)
  | Array(t, l) ->
      sprintf "Array(%s, [%s])"
        (OBus_types.string_of_single t)
        (String.concat "; " (List.map string_of_single l))
  | Dict(tk, tv, l) ->
      sprintf "Dict(%s, %s, [%s])"
        (OBus_types.string_of_basic tk)
        (OBus_types.string_of_single tv)
        (String.concat "; "
           (List.map (fun (k, v) -> sprintf "(%s, %s)"
                        (string_of_basic k)
                        (string_of_single v)) l))
  | Struct l -> sprintf "Structure %s" (string_of_sequence l)
  | Variant x -> sprintf "Variant(%s)" (string_of_single x)

and string_of_sequence l = sprintf "[%s]" (String.concat "; " (List.map string_of_single l))
