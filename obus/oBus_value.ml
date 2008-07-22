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

type ('a, 'b, 'c) ty_desc = {
  make : 'a -> 'b;
  cast : 'b -> 'a;
  typ : 'c;
}

type ('a, 'cl) ty =
  | Ty_basic of ('a, basic, OBus_types.basic) ty_desc
  | Ty_single of ('a, single, OBus_types.single) ty_desc
  | Ty_sequence of ('a, sequence, OBus_types.sequence) ty_desc

type cl_basic = [ `basic | `single | `sequence ]
type cl_single = [ `single | `sequence ]
type cl_sequence = [ `sequence ]

let make_basic ty v = match ty with
  | Ty_basic { make = f } -> f v
  | _ -> assert false
let make_single ty v = match ty with
  | Ty_basic { make = f } -> Basic(f v)
  | Ty_single { make = f } -> f v
  | _ -> assert false
let make_sequence ty v = match ty with
  | Ty_basic { make = f } -> [Basic(f v)]
  | Ty_single { make = f } -> [f v]
  | Ty_sequence { make = f } -> f v

exception Cast_failure

let cast_basic ty v = match ty with
  | Ty_basic { cast = f } -> f v
  | _ -> raise Cast_failure
let cast_single ty v = match ty, v with
  | Ty_basic { cast = f }, Basic x -> f x
  | Ty_single { cast = f }, x -> f x
  | _ -> raise Cast_failure
let cast_sequence ty v = match ty, v with
  | Ty_basic { cast = f }, [Basic x] -> f x
  | Ty_single { cast = f }, [x] -> f x
  | Ty_sequence { cast = f }, x -> f x
  | _ -> raise Cast_failure

let opt_cast cast ty v =
  try
    Some(cast ty v)
  with
    | Cast_failure -> None

let opt_cast_basic ty v = opt_cast cast_basic ty v
let opt_cast_single ty v = opt_cast cast_single ty v
let opt_cast_sequence ty v = opt_cast cast_sequence ty v

let type_of_basic_ty = function
  | Ty_basic { typ = t } -> t
  | _ -> assert false
let type_of_single_ty = function
  | Ty_basic { typ = t } -> Tbasic t
  | Ty_single { typ = t } -> t
  | _ -> assert false
let type_of_sequence_ty = function
  | Ty_basic { typ = t } -> [Tbasic t]
  | Ty_single { typ = t } -> [t]
  | Ty_sequence { typ = t } -> t

let tbyte = Ty_basic { make = (fun x -> Byte x);
                       cast = (function
                                 | Byte x -> x
                                 | _ -> raise Cast_failure);
                       typ = Tbyte }
let tboolean = Ty_basic { make = (fun x -> Boolean x);
                          cast = (function
                                    | Boolean x -> x
                                    | _ -> raise Cast_failure);
                          typ = Tboolean }
let tint16 = Ty_basic { make = (fun x -> Int16 x);
                        cast = (function
                                  | Int16 x -> x
                                  | _ -> raise Cast_failure);
                        typ = Tint16 }
let tint32 = Ty_basic { make = (fun x -> Int32 x);
                        cast = (function
                                  | Int32 x -> x
                                  | _ -> raise Cast_failure);
                        typ = Tint32 }
let tint64 = Ty_basic { make = (fun x -> Int64 x);
                        cast = (function
                                  | Int64 x -> x
                                  | _ -> raise Cast_failure);
                        typ = Tint64 }
let tuint16 = Ty_basic { make = (fun x -> Uint16 x);
                         cast = (function
                                   | Uint16 x -> x
                                   | _ -> raise Cast_failure);
                         typ = Tuint16 }
let tuint32 = Ty_basic { make = (fun x -> Uint32 x);
                         cast = (function
                                   | Uint32 x -> x
                                   | _ -> raise Cast_failure);
                         typ = Tuint32 }
let tuint64 = Ty_basic { make = (fun x -> Uint64 x);
                         cast = (function
                                   | Uint64 x -> x
                                   | _ -> raise Cast_failure);
                         typ = Tuint64 }
let tdouble = Ty_basic { make = (fun x -> Double x);
                         cast = (function
                                   | Double x -> x
                                   | _ -> raise Cast_failure);
                         typ = Tdouble }
let tstring = Ty_basic { make = (fun x -> String x);
                         cast = (function
                                   | String x -> x
                                   | _ -> raise Cast_failure);
                         typ = Tstring }
let tsignature = Ty_basic { make = (fun x -> Signature x);
                            cast = (function
                                      | Signature x -> x
                                      | _ -> raise Cast_failure);
                            typ = Tsignature }
let tobject_path = Ty_basic { make = (fun x -> Object_path x);
                              cast = (function
                                        | Object_path x -> x
                                        | _ -> raise Cast_failure);
                              typ = Tobject_path }
let tarray ty =
  let t = type_of_single_ty ty in
    Ty_single { make = (fun l -> Array(t, List.map (make_single ty) l));
                cast = (function
                          | Array(t', l) when t' = t ->
                              List.map (cast_single ty) l
                          | _ -> raise Cast_failure);
                typ = Tarray t }
let tdict tyk tyv =
  let tk = type_of_basic_ty tyk and tv = type_of_single_ty tyv in
    Ty_single { make = (fun l -> Dict(tk, tv, List.map (fun (k, v) -> make_basic tyk k, make_single tyv v) l));
                cast = (function
                          | Dict(tk', tv', l) when tk' = tk && tv' = tv ->
                              List.map (fun (k, v) -> cast_basic tyk k, cast_single tyv v) l
                          | _ -> raise Cast_failure);
                typ = Tdict(tk, tv) }
let tstruct ty =
  Ty_single { make = (fun l -> Struct (make_sequence ty l));
              cast = (function
                        | Struct l -> cast_sequence ty l
                        | _ -> raise Cast_failure);
              typ = Tstruct (type_of_sequence_ty ty) }
let tvariant =
  Ty_single { make = (fun v -> Variant v);
               cast = (function
                         | Variant v -> v
                         | _ -> raise Cast_failure);
               typ = Tvariant }
let tnil =
  Ty_sequence { make = (fun () -> []);
                cast = (function
                          | [] -> ()
                          | _ -> raise Cast_failure);
                typ = [] }

let tcons tyx tyl =
  Ty_sequence { make = (fun (x, l) -> make_single tyx x :: make_sequence tyl l);
                cast = (function
                          | x :: l ->
                              let a = cast_single tyx x
                              and b = cast_sequence tyl l in
                                (a, b)
                          | [] -> raise Cast_failure);
                typ = type_of_single_ty tyx :: type_of_sequence_ty tyl }

let tup0 = tnil
let tup1 (ty1 : ('a, [> cl_single ]) ty) = (ty1 :> ('a, cl_sequence) ty)
let tup2 ty1 ty2 =
  Ty_sequence { make = (fun (x1, x2) ->
                          [make_single ty1 x1;
                           make_single ty2 x2]);
                cast = (function
                          | [x1; x2] ->
                              let v1 = cast_single ty1 x1
                              and v2 = cast_single ty2 x2 in
                                (v1, v2)
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2] }
let tup3 ty1 ty2 ty3 =
  Ty_sequence { make = (fun (x1, x2, x3) ->
                          [make_single ty1 x1;
                           make_single ty2 x2;
                           make_single ty3 x3]);
                cast = (function
                          | [x1; x2; x3] ->
                              let v1 = cast_single ty1 x1
                              and v2 = cast_single ty2 x2
                              and v3 = cast_single ty3 x3 in
                                (v1, v2, v3)
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2;
                       type_of_single_ty ty3] }
let tup4 ty1 ty2 ty3 ty4 =
  Ty_sequence { make = (fun (x1, x2, x3, x4) ->
                          [make_single ty1 x1;
                           make_single ty2 x2;
                           make_single ty3 x3;
                           make_single ty4 x4]);
                cast = (function
                          | [x1; x2; x3; x4] ->
                              let v1 = cast_single ty1 x1
                              and v2 = cast_single ty2 x2
                              and v3 = cast_single ty3 x3
                              and v4 = cast_single ty4 x4 in
                                (v1, v2, v3, v4)
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2;
                       type_of_single_ty ty3;
                       type_of_single_ty ty4] }
let tup5 ty1 ty2 ty3 ty4 ty5 =
  Ty_sequence { make = (fun (x1, x2, x3, x4, x5) ->
                          [make_single ty1 x1;
                           make_single ty2 x2;
                           make_single ty3 x3;
                           make_single ty4 x4;
                           make_single ty5 x5]);
                cast = (function
                          | [x1; x2; x3; x4; x5] ->
                              let v1 = cast_single ty1 x1
                              and v2 = cast_single ty2 x2
                              and v3 = cast_single ty3 x3
                              and v4 = cast_single ty4 x4
                              and v5 = cast_single ty5 x5 in
                                (v1, v2, v3, v4, v5)
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2;
                       type_of_single_ty ty3;
                       type_of_single_ty ty4;
                       type_of_single_ty ty5] }
let tup6 ty1 ty2 ty3 ty4 ty5 ty6 =
  Ty_sequence { make = (fun (x1, x2, x3, x4, x5, x6) ->
                          [make_single ty1 x1;
                           make_single ty2 x2;
                           make_single ty3 x3;
                           make_single ty4 x4;
                           make_single ty5 x5;
                           make_single ty6 x6]);
                cast = (function
                          | [x1; x2; x3; x4; x5; x6] ->
                              let v1 = cast_single ty1 x1
                              and v2 = cast_single ty2 x2
                              and v3 = cast_single ty3 x3
                              and v4 = cast_single ty4 x4
                              and v5 = cast_single ty5 x5
                              and v6 = cast_single ty6 x6 in
                                (v1, v2, v3, v4, v5, v6)
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2;
                       type_of_single_ty ty3;
                       type_of_single_ty ty4;
                       type_of_single_ty ty5;
                       type_of_single_ty ty6] }
let tup7 ty1 ty2 ty3 ty4 ty5 ty6 ty7 =
  Ty_sequence { make = (fun (x1, x2, x3, x4, x5, x6, x7) ->
                          [make_single ty1 x1;
                           make_single ty2 x2;
                           make_single ty3 x3;
                           make_single ty4 x4;
                           make_single ty5 x5;
                           make_single ty6 x6;
                           make_single ty7 x7]);
                cast = (function
                          | [x1; x2; x3; x4; x5; x6; x7] ->
                              let v1 = cast_single ty1 x1
                              and v2 = cast_single ty2 x2
                              and v3 = cast_single ty3 x3
                              and v4 = cast_single ty4 x4
                              and v5 = cast_single ty5 x5
                              and v6 = cast_single ty6 x6
                              and v7 = cast_single ty7 x7 in
                                (v1, v2, v3, v4, v5, v6, v7)
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2;
                       type_of_single_ty ty3;
                       type_of_single_ty ty4;
                       type_of_single_ty ty5;
                       type_of_single_ty ty6;
                       type_of_single_ty ty7] }
let tup8 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 =
  Ty_sequence { make = (fun (x1, x2, x3, x4, x5, x6, x7, x8) ->
                          [make_single ty1 x1;
                           make_single ty2 x2;
                           make_single ty3 x3;
                           make_single ty4 x4;
                           make_single ty5 x5;
                           make_single ty6 x6;
                           make_single ty7 x7;
                           make_single ty8 x8]);
                cast = (function
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
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2;
                       type_of_single_ty ty3;
                       type_of_single_ty ty4;
                       type_of_single_ty ty5;
                       type_of_single_ty ty6;
                       type_of_single_ty ty7;
                       type_of_single_ty ty8] }
let tup9 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 ty9 =
  Ty_sequence { make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
                          [make_single ty1 x1;
                           make_single ty2 x2;
                           make_single ty3 x3;
                           make_single ty4 x4;
                           make_single ty5 x5;
                           make_single ty6 x6;
                           make_single ty7 x7;
                           make_single ty8 x8;
                           make_single ty9 x9]);
                cast = (function
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
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2;
                       type_of_single_ty ty3;
                       type_of_single_ty ty4;
                       type_of_single_ty ty5;
                       type_of_single_ty ty6;
                       type_of_single_ty ty7;
                       type_of_single_ty ty8;
                       type_of_single_ty ty9] }
let tup10 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 ty9 ty10 =
  Ty_sequence { make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) ->
                          [make_single ty1 x1;
                           make_single ty2 x2;
                           make_single ty3 x3;
                           make_single ty4 x4;
                           make_single ty5 x5;
                           make_single ty6 x6;
                           make_single ty7 x7;
                           make_single ty8 x8;
                           make_single ty9 x9;
                           make_single ty10 x10]);
                cast = (function
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
                          | _ -> raise Cast_failure);
                typ = [type_of_single_ty ty1;
                       type_of_single_ty ty2;
                       type_of_single_ty ty3;
                       type_of_single_ty ty4;
                       type_of_single_ty ty5;
                       type_of_single_ty ty6;
                       type_of_single_ty ty7;
                       type_of_single_ty ty8;
                       type_of_single_ty ty9;
                       type_of_single_ty ty10] }

type ('b, 'c) with_ty = { with_ty : 'a. ('a, 'b) ty -> 'c }

let with_basic_ty w = function
  | Tbyte -> w.with_ty tbyte
  | Tboolean -> w.with_ty tboolean
  | Tint16 -> w.with_ty tint16
  | Tint32 -> w.with_ty tint32
  | Tint64 -> w.with_ty tint64
  | Tuint16 -> w.with_ty tuint16
  | Tuint32 -> w.with_ty tuint32
  | Tuint64 -> w.with_ty tuint64
  | Tdouble -> w.with_ty tdouble
  | Tstring -> w.with_ty tstring
  | Tsignature -> w.with_ty tsignature
  | Tobject_path -> w.with_ty tobject_path

let rec with_single_ty w = function
  | Tbasic t -> with_basic_ty w t
  | Tarray t -> with_single_ty { with_ty = fun t -> w.with_ty (tarray t) } t
  | Tdict(tk, tv) -> with_basic_ty { with_ty = fun tk ->
                                       with_single_ty { with_ty = fun tv -> w.with_ty (tdict tk tv) } tv } tk
  | Tstruct tl -> with_sequence_ty { with_ty = fun t -> w.with_ty (tstruct t) } tl
  | Tvariant -> w.with_ty tvariant

and with_sequence_ty w = function
  | [] -> w.with_ty tnil
  | tx :: tl -> with_single_ty { with_ty = fun tx ->
                                   with_sequence_ty { with_ty = fun tl -> w.with_ty (tcons tx tl) } tl } tx

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
