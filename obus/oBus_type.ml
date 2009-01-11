(*
 * oBus_type.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_value

(***** Sequence type as tree *****)

(* Type description involve a lot of small concatenation, especially
   in functionnal types, and most of the time we will get the type as
   a list only one time (in OBus_connection.send_...), so to avoid
   multiple list concatenation we use this intermediate
   representation: *)

type 'a tree =
  | Tcons of 'a tree * 'a tree
  | Tone of 'a
  | Tnil

(* Compute the boundary of a tree *)
let tree_get_boundary t =
  let rec aux acc = function
    | Tone t -> t :: acc
    | Tcons(x, y) -> aux (aux acc y) x
    | Tnil -> acc
  in
  aux [] t

(***** Type combinators *****)

type context = exn

type ('a, 'typ, 'make, 'cast) ty_desc = {
  (* The dbus type *)
  dtype : 'typ;

  (* Value functions *)
  make : 'make;
  cast : context -> 'cast;
}

type 'a ty_desc_basic = ('a, tbasic, 'a -> basic, basic -> 'a) ty_desc
type 'a ty_desc_single =  ('a, tsingle, 'a -> single, single -> 'a) ty_desc
type 'a ty_desc_element =  ('a, telement, 'a -> element, element -> 'a) ty_desc
type 'a ty_desc_sequence = ('a, tsingle tree, 'a -> single tree, sequence -> 'a * sequence) ty_desc

type 'a ty_basic = [ `basic of 'a ty_desc_basic ]
type 'a ty_single = [ `single of 'a ty_desc_single ]
type 'a ty_element = [ `element of 'a ty_desc_element ]
type 'a ty_sequence = [ `sequence of 'a ty_desc_sequence ]

type 'a cl_basic = [ 'a ty_basic ]
type 'a cl_single = [ 'a cl_basic | 'a ty_single ]
type 'a cl_element = [ 'a cl_single | 'a ty_element ]
type 'a cl_sequence = [ 'a cl_single | 'a ty_sequence ]

type ('a, 'b, 'c) ty_function = {
  ftype : tsingle tree;
  fmake : single tree -> (sequence -> 'b) -> 'a;
  fcast : context -> sequence -> 'a -> 'b;
  freply : 'c cl_sequence;
}

let type_basic (`basic { dtype = t }) = t
let type_single = function
  | `basic { dtype = t } -> Tbasic t
  | `single { dtype = t } -> t
let type_element = function
  | `basic { dtype = t } -> Tsingle (Tbasic t)
  | `single { dtype = t } -> Tsingle t
  | `element { dtype = t } -> t
let type_sequence = function
  | `basic { dtype = t } -> [Tbasic t]
  | `single { dtype = t } -> [t]
  | `sequence { dtype = t } -> tree_get_boundary t

let isignature { ftype = s } = tree_get_boundary s
let osignature { freply = r } = type_sequence r

let make_basic (`basic { make = f }) = f
let make_single = function
  | `basic { make = f } -> (fun x -> vbasic(f x))
  | `single { make = f } -> f
let make_element =function
  | `basic { make = f } -> (fun x -> Single(vbasic(f x)))
  | `single { make = f } -> (fun x -> Single(f x))
  | `element { make = f } -> f
let make_sequence = function
  | `basic { make = f } -> (fun x -> [vbasic(f x)])
  | `single { make = f } -> (fun x -> [f x])
  | `sequence { make = f } -> (fun x -> tree_get_boundary (f x))

exception Cast_failure

let _cast_basic (`basic { cast = f }) = f
let _cast_single = function
  | `basic { cast = f } ->
      (fun context -> function
         | Basic x -> f context x
         | _ -> raise Cast_failure)
  | `single { cast = f } -> f
let _cast_element = function
  | `basic { cast = f } ->
      (fun context -> function
         | Single(Basic x) -> f context x
         | _ -> raise Cast_failure)
  | `single { cast = f } ->
      (fun context -> function
         | Single x -> f context x
         | _ -> raise Cast_failure)
  | `element { cast = f } -> f
let _cast_sequence = function
  | `basic { cast = f } ->
      (fun context -> function
         | [Basic x] -> f context x
         | _ -> raise Cast_failure)
  | `single { cast = f } ->
      (fun context -> function
         | [x] -> f context x
         | _ -> raise Cast_failure)
  | `sequence { cast = f } ->
      (fun context -> fun l -> match f context l with
         | v, [] -> v
         | _ -> raise Cast_failure)

exception No_context

let cast_basic ty ?(context=No_context) x = _cast_basic ty context x
let cast_single ty ?(context=No_context) x = _cast_single ty context x
let cast_element ty ?(context=No_context) x = _cast_element ty context x
let cast_sequence ty ?(context=No_context) x = _cast_sequence ty context x

let opt_cast f ?(context=No_context) x = try Some(f context x) with Cast_failure -> None
let opt_cast_basic t = opt_cast (_cast_basic t)
let opt_cast_single t = opt_cast (_cast_single t)
let opt_cast_element t = opt_cast (_cast_element t)
let opt_cast_sequence t = opt_cast (_cast_sequence t)

let make_func { fmake = f } cont = f Tnil cont
let cast_func { fcast = f } ?(context=No_context) x g = f context x g
let opt_cast_func { fcast = f } ?(context=No_context) x g =
  try
    Some(f context x g)
  with
      Cast_failure -> None

let func_reply { freply = r } = r

let wrap t f g = {
  dtype = t.dtype;
  make = (fun x -> t.make (g x));
  cast = (fun context x -> f (t.cast context x));
}

let wrap_basic (`basic t) f g = `basic (wrap t f g)
let wrap_single (`single t) f g = `single (wrap t f g)
let wrap_element (`element t) f g = `element (wrap t f g)
let wrap_sequence (`sequence t) f g = `sequence
  { dtype = t.dtype;
    make = (fun x -> t.make (g x));
    cast = (fun context l -> let v, rest = t.cast context l in (f v, rest)) }

let wrap_ctx t f g = {
  dtype = t.dtype;
  make = (fun x -> t.make (g x));
  cast = (fun context x -> f context (t.cast context x));
}

let wrap_basic_ctx (`basic t) f g = `basic (wrap_ctx t f g)
let wrap_single_ctx (`single t) f g = `single (wrap_ctx t f g)
let wrap_element_ctx (`element t) f g = `element (wrap_ctx t f g)
let wrap_sequence_ctx (`sequence t) f g = `sequence
  { dtype = t.dtype;
    make = (fun x -> t.make (g x));
    cast = (fun context l -> let v, rest = t.cast context l in (f context v, rest)) }

let wrap_array elt ~make ~cast =
  let typ = type_element elt in
  `single
    { dtype = Tarray typ;
      make = (let f = make_element elt in
              fun x -> varray typ (make f x));
      cast = (let f = _cast_element elt in
              fun context -> function
                | Array(t, l) when t = typ -> cast (f context) l
                | _ -> raise Cast_failure) }

let wrap_array_ctx elt ~make ~cast =
  let typ = type_element elt in
  `single
    { dtype = Tarray typ;
      make = (let f = make_element elt in
              fun x -> varray typ (make f x));
      cast = (let f = _cast_element elt in
              fun context -> function
                | Array(t, l) when t = typ -> cast context (f context) l
                | _ -> raise Cast_failure) }

(***** Predefined types *****)

module Pervasives =
struct

  let tbyte = `basic
    { dtype = Tbyte;
      make = vbyte;
      cast = (fun context -> function
                | Byte x -> x
                | _ -> raise Cast_failure) }

  let tchar = tbyte

  let tint8 = wrap_basic tbyte
    (fun x -> let x = int_of_char x in
     if x >= 128 then x - 256 else x)
    (fun x -> Char.unsafe_chr (x land 0xff))

  let tuint8 = wrap_basic tbyte
    int_of_char
    (fun x -> Char.unsafe_chr (x land 0xff))

  let tboolean = `basic
    { dtype = Tboolean;
      make = vboolean;
      cast = (fun context -> function
                | Boolean x -> x
                | _ -> raise Cast_failure) }

  let tbool = tboolean

  let tint16 = `basic
    { dtype = Tint16;
      make = vint16;
      cast = (fun context -> function
                | Int16 x -> x
                | _ -> raise Cast_failure) }

  let tint32 = `basic
    { dtype = Tint32;
      make = vint32;
      cast = (fun context -> function
                | Int32 x -> x
                | _ -> raise Cast_failure) }

  let tint = wrap_basic tint32 Int32.to_int Int32.of_int

  let tint64 = `basic
    { dtype = Tint64;
      make = vint64;
      cast = (fun context -> function
                | Int64 x -> x
                | _ -> raise Cast_failure) }

  let tuint16 = `basic
    { dtype = Tuint16;
      make = vuint16;
      cast = (fun context -> function
                | Uint16 x -> x
                | _ -> raise Cast_failure) }

  let tuint32 = `basic
    { dtype = Tuint32;
      make = vuint32;
      cast = (fun context -> function
                | Uint32 x -> x
                | _ -> raise Cast_failure) }

  let tuint = wrap_basic tuint32 Int32.to_int Int32.of_int

  let tuint64 = `basic
    { dtype = Tuint64;
      make = vuint64;
      cast = (fun context -> function
                | Uint64 x -> x
                | _ -> raise Cast_failure) }

  let tdouble = `basic
    { dtype = Tdouble;
      make = vdouble;
      cast = (fun context -> function
                | Double x -> x
                | _ -> raise Cast_failure) }

  let tfloat = tdouble

  let tstring = `basic
    { dtype = Tstring;
      make = vstring;
      cast = (fun context -> function
                | String x -> x
                | _ -> raise Cast_failure) }

  let tsignature = `basic
    { dtype = Tsignature;
      make = vsignature;
      cast = (fun context -> function
                | Signature x -> x
                | _ -> raise Cast_failure) }

  let tobject_path = `basic
    { dtype = Tobject_path;
      make = vobject_path;
      cast = (fun context -> function
                | Object_path x -> x
                | _ -> raise Cast_failure) }

  let tpath = tobject_path

  let tlist elt = wrap_array elt List.map List.map

  let tbyte_array = wrap_array tbyte
    (fun f str ->
       let rec aux i acc =
         if i = 0
         then acc
         else aux (i - 1) (f str.[i] :: acc)
       in
       aux (String.length str) [])
    (fun f l ->
       let len = List.length l in
       let str = String.create len in
       ignore (List.fold_left (fun i x -> String.unsafe_set str i (f x); i + 1) 0 l);
       str)

  let tdict_entry tyk tyv =
    let ktyp = type_basic tyk
    and vtyp = type_single tyv in
    `element
      { dtype = Tdict_entry(ktyp, vtyp);
        make = (let f = make_basic tyk
                and g = make_single tyv in
                fun (k, v) -> Dict_entry(f k, g v));
        cast = (let f = _cast_basic tyk
                and g = _cast_single tyv in
                fun context -> function
                  | Dict_entry(k, v) -> (f context k, g context v)
                  | _ -> raise Cast_failure) }

  let tassoc tyk tyv = tlist (tdict_entry tyk tyv)

  let tstructure ty = `single
    { dtype = Tstruct(type_sequence ty);
      make = (let f = make_sequence ty in
              fun x -> vstruct (f x));
      cast = (let f = _cast_sequence ty in
              fun context -> function
                | Struct l -> f context l
                | _ -> raise Cast_failure) }

  let tvariant = `single
    { dtype = Tvariant;
      make = vvariant;
      cast = (fun context -> function
                | Variant v -> v
                | _ -> raise Cast_failure) }

  let tunit = `sequence
    { dtype = Tnil;
      make = (fun () -> Tnil);
      cast = (fun context l -> ((), l)) }

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
  type 'a set = 'a list
  type ('a, 'b) dict_entry = 'a * 'b
  type ('a, 'b) assoc = ('a, 'b) dict_entry set
  type 'a structure = 'a
  type variant = single
  type byte_array = string
end

open Pervasives

module type Ordered_element_type = sig
  type t
  val tt : t cl_element
  val compare : t -> t -> int
end

module type Ordered_basic_type = sig
  type t
  val tt : t cl_basic
  val compare : t -> t -> int
end

module Make_set(Ord : Ordered_element_type) =
struct
  include Set.Make(Ord)
  let tt = wrap_array Ord.tt
    ~make:(fun f set -> fold (fun x acc -> f x :: acc) set [])
    ~cast:(fun f l -> List.fold_left (fun acc x -> add (f x) acc) empty l)
end

module Make_map(Ord : Ordered_basic_type) =
struct
  include Map.Make(Ord)
  let tt ty = wrap_array (tdict_entry Ord.tt ty)
    ~make:(fun f map -> fold (fun k v acc -> f (k, v) :: acc) map [])
    ~cast:(fun f l -> List.fold_left (fun acc x ->
                                        let (k, v) = f x in
                                        add k v acc) empty l)
end

let (@) a b = Tcons(a, b)
let typ = function
  | `basic { dtype = t } -> Tone (Tbasic t)
  | `single { dtype = t } -> Tone t
  | `sequence { dtype = t } -> t
let make ty v = match ty with
  | `basic { make = f } -> Tone (vbasic(f v))
  | `single { make = f } -> Tone (f v)
  | `sequence { make = f } -> f v
let cast ty context x = match ty, x with
  | `basic { cast = f }, Basic x :: l -> f context x, l
  | `single { cast = f }, x :: l -> f context x, l
  | `sequence { cast = f }, x -> f context x
  | #cl_sequence, _ -> raise Cast_failure

let reply ty =
  { ftype = Tnil;
    fmake = (fun acc cont -> cont (tree_get_boundary acc));
    fcast = (fun context -> function
               | [] -> (fun f -> f)
               | _ -> raise Cast_failure);
    freply = (ty :> _ cl_sequence) }

let abstract ty fty =
  { ftype = typ ty @ fty.ftype;
    fmake = (fun acc cont x -> fty.fmake (Tcons(acc, make ty x)) cont);
    fcast = (fun context x ->
               let x, rest = cast ty context x in
               let f = fty.fcast context rest in
               fun g -> f (g x));
    freply = fty.freply }

let (-->) = abstract

let tup2 ty1 ty2 = `sequence
  { dtype = typ ty1 @ typ ty2;
    make = (fun (x1, x2) ->
              make ty1 x1
              @ make ty2 x2);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              ((v1, v2), l)) }

let tup3 ty1 ty2 ty3 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3;
    make = (fun (x1, x2, x3) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              let v3, l = cast ty3 context l in
              ((v1, v2, v3), l)) }

let tup4 ty1 ty2 ty3 ty4 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4;
    make = (fun (x1, x2, x3, x4) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              let v3, l = cast ty3 context l in
              let v4, l = cast ty4 context l in
              ((v1, v2, v3, v4), l)) }

let tup5 ty1 ty2 ty3 ty4 ty5 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5;
    make = (fun (x1, x2, x3, x4, x5) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              let v3, l = cast ty3 context l in
              let v4, l = cast ty4 context l in
              let v5, l = cast ty5 context l in
              ((v1, v2, v3, v4, v5), l)) }

let tup6 ty1 ty2 ty3 ty4 ty5 ty6 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6;
    make = (fun (x1, x2, x3, x4, x5, x6) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              let v3, l = cast ty3 context l in
              let v4, l = cast ty4 context l in
              let v5, l = cast ty5 context l in
              let v6, l = cast ty6 context l in
              ((v1, v2, v3, v4, v5, v6), l)) }

let tup7 ty1 ty2 ty3 ty4 ty5 ty6 ty7 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6 @ typ ty7;
    make = (fun (x1, x2, x3, x4, x5, x6, x7) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6
              @ make ty7 x7);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              let v3, l = cast ty3 context l in
              let v4, l = cast ty4 context l in
              let v5, l = cast ty5 context l in
              let v6, l = cast ty6 context l in
              let v7, l = cast ty7 context l in
              ((v1, v2, v3, v4, v5, v6, v7), l)) }

let tup8 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6 @ typ ty7 @ typ ty8;
    make = (fun (x1, x2, x3, x4, x5, x6, x7, x8) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6
              @ make ty7 x7
              @ make ty8 x8);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              let v3, l = cast ty3 context l in
              let v4, l = cast ty4 context l in
              let v5, l = cast ty5 context l in
              let v6, l = cast ty6 context l in
              let v7, l = cast ty7 context l in
              let v8, l = cast ty8 context l in
              ((v1, v2, v3, v4, v5, v6, v7, v8), l)) }

let tup9 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 ty9 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6 @ typ ty7 @ typ ty8 @ typ ty9;
    make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6
              @ make ty7 x7
              @ make ty8 x8
              @ make ty9 x9);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              let v3, l = cast ty3 context l in
              let v4, l = cast ty4 context l in
              let v5, l = cast ty5 context l in
              let v6, l = cast ty6 context l in
              let v7, l = cast ty7 context l in
              let v8, l = cast ty8 context l in
              let v9, l = cast ty9 context l in
              ((v1, v2, v3, v4, v5, v6, v7, v8, v9), l)) }

let tup10 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 ty9 ty10 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6 @ typ ty7 @ typ ty8 @ typ ty9 @ typ ty10;
    make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6
              @ make ty7 x7
              @ make ty8 x8
              @ make ty9 x9
              @ make ty10 x10);
    cast = (fun context l ->
              let v1, l = cast ty1 context l in
              let v2, l = cast ty2 context l in
              let v3, l = cast ty3 context l in
              let v4, l = cast ty4 context l in
              let v5, l = cast ty5 context l in
              let v6, l = cast ty6 context l in
              let v7, l = cast ty7 context l in
              let v8, l = cast ty8 context l in
              let v9, l = cast ty9 context l in
              let v10, l = cast ty10 context l in
              ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10), l)) }

type 'a with_ty_basic = { with_ty_basic : 'b. 'b ty_basic -> 'a }
type 'a with_ty_single = { with_ty_single : 'b. 'b ty_single -> 'a }
type 'a with_ty_element = { with_ty_element : 'b. 'b ty_element -> 'a }
type 'a with_ty_sequence = { with_ty_sequence : 'b. 'b ty_sequence -> 'a }

let with_ty_basic w = function
  | Tbyte -> w.with_ty_basic tbyte
  | Tboolean -> w.with_ty_basic tboolean
  | Tint16 -> w.with_ty_basic tint16
  | Tint32 -> w.with_ty_basic tint32
  | Tint64 -> w.with_ty_basic tint64
  | Tuint16 -> w.with_ty_basic tuint16
  | Tuint32 -> w.with_ty_basic tuint32
  | Tuint64 -> w.with_ty_basic tuint64
  | Tdouble -> w.with_ty_basic tdouble
  | Tstring -> w.with_ty_basic tstring
  | Tsignature -> w.with_ty_basic tsignature
  | Tobject_path -> w.with_ty_basic tobject_path

let single_of_basic (`basic t) = `single
  { dtype = Tbasic t.dtype;
    make = (fun x -> vbasic(t.make x));
    cast = (fun context -> function
              | Basic x -> t.cast context x
              | _ -> raise Cast_failure) }

let element_of_single (`single t) = `element
  { dtype = Tsingle t.dtype;
    make = (fun x -> Single(t.make x));
    cast = (fun context -> function
              | Single x -> t.cast context x
              | _ -> raise Cast_failure) }

let rec with_ty_single w = function
  | Tbasic t -> with_ty_basic { with_ty_basic = fun t -> w.with_ty_single (single_of_basic t) } t
  | Tarray t -> with_ty_element { with_ty_element = fun t -> w.with_ty_single (tlist t) } t
  | Tstruct tl -> with_ty_sequence { with_ty_sequence = fun t -> w.with_ty_single (tstructure t) } tl
  | Tvariant -> w.with_ty_single tvariant

and with_ty_element w = function
  | Tdict_entry(tk, tv) ->
      with_ty_basic { with_ty_basic = fun tk ->
                        with_ty_single { with_ty_single = fun tv -> w.with_ty_element (tdict_entry tk tv) } tv } tk
  | Tsingle t ->
      with_ty_single { with_ty_single = fun t ->
                         w.with_ty_element (element_of_single t) } t

and with_ty_sequence w = function
  | [] -> w.with_ty_sequence tunit
  | tx :: tl -> with_ty_single
      { with_ty_single = fun tx ->
          with_ty_sequence
            { with_ty_sequence = fun tl -> w.with_ty_sequence
                (tup2 (tx :> _ cl_sequence) (tl :> _ cl_sequence)) } tl } tx
