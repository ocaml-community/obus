(*
 * oBus_type.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_value

(* +-----------------------+
   | Sequence type as tree |
   +-----------------------+ *)

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

(* +------------------+
   | Type combinators |
   +------------------+ *)

type context = exn

type ('typ, 'make, 'cast) type_combinator = {
  (* The dbus type *)
  dtype : 'typ;

  (* Value functions *)
  make : 'make;
  cast : context -> 'cast;
}

type ('a, 'cl) t =
  | Cbasic of (tbasic, 'a -> basic, basic -> 'a) type_combinator
  | Ccontainer of (tsingle, 'a -> single, single -> 'a) type_combinator
  | Csequence of (tsingle tree, 'a -> single tree, sequence -> 'a * sequence) type_combinator

type 'a basic = ('a, [`basic]) t
type 'a container = ('a, [`container]) t
type 'a sequence = ('a, [`sequence]) t

type ('a, 'cl) cl_basic = ('a, 'cl) t
constraint 'cl = [ `basic ]

type ('a, 'cl) cl_single = ('a, 'cl) t
constraint 'cl = [< `basic | `container ]

type ('a, 'cl) cl_sequence = ('a, 'cl) t
constraint 'cl = [< `basic | `container | `sequence ]

type ('a, 'b, 'c) func = {
  ftype : OBus_value.tsingle tree;
  fmake : OBus_value.single tree -> (OBus_value.sequence -> 'b) -> 'a;
  fcast : context -> OBus_value.sequence -> 'a -> 'b;
  freply : 'c sequence;
}

let type_basic = function
  | Cbasic { dtype = t } -> t
  | _ -> assert false
let type_single = function
  | Cbasic { dtype = t } -> Tbasic t
  | Ccontainer { dtype = t } -> t
  | _ -> assert false
let type_sequence = function
  | Cbasic { dtype = t } -> [Tbasic t]
  | Ccontainer { dtype = t } -> [t]
  | Csequence { dtype = t } -> tree_get_boundary t

let isignature { ftype = s } = tree_get_boundary s
let osignature { freply = r } = type_sequence r

let make_basic = function
  | Cbasic { make = f } -> f
  | _ -> assert false
let make_single = function
  | Cbasic { make = f } -> (fun x -> basic(f x))
  | Ccontainer { make = f } -> f
  | _ -> assert false
let make_sequence = function
  | Cbasic { make = f } -> (fun x -> [basic(f x)])
  | Ccontainer { make = f } -> (fun x -> [f x])
  | Csequence { make = f } -> (fun x -> tree_get_boundary (f x))

exception Cast_failure

let _cast_basic = function
  | Cbasic { cast = f } -> f
  | _ -> assert false
let _cast_single = function
  | Cbasic { cast = f } ->
      (fun context -> function
         | Basic x -> f context x
         | _ -> raise Cast_failure)
  | Ccontainer { cast = f } -> f
  | _ -> assert false
let _cast_sequence = function
  | Cbasic { cast = f } ->
      (fun context -> function
         | [Basic x] -> f context x
         | _ -> raise Cast_failure)
  | Ccontainer { cast = f } ->
      (fun context -> function
         | [x] -> f context x
         | _ -> raise Cast_failure)
  | Csequence { cast = f } ->
      (fun context -> fun l -> match f context l with
         | v, [] -> v
         | _ -> raise Cast_failure)

exception No_context

let cast_basic t ?(context=No_context) x = _cast_basic t context x
let cast_single t ?(context=No_context) x = _cast_single t context x
let cast_sequence t ?(context=No_context) x = _cast_sequence t context x

let opt_cast f ?(context=No_context) x = try Some(f context x) with Cast_failure -> None
let opt_cast_basic t = opt_cast (_cast_basic t)
let opt_cast_single t = opt_cast (_cast_single t)
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

let wrap t f g = match t with
  | Cbasic t -> Cbasic(wrap t f g)
  | Ccontainer t -> Ccontainer(wrap t f g)
  | Csequence t -> Csequence{
      dtype = t.dtype;
      make = (fun x -> t.make (g x));
      cast = (fun context l -> let v, rest = t.cast context l in (f v, rest))
    }

let wrap_with_context t f g = {
  dtype = t.dtype;
  make = (fun x -> t.make (g x));
  cast = (fun context x -> f context (t.cast context x));
}

let wrap_with_context t f g = match t with
  | Cbasic t -> Cbasic(wrap_with_context t f g)
  | Ccontainer t -> Ccontainer(wrap_with_context t f g)
  | Csequence t -> Csequence{
      dtype = t.dtype;
      make = (fun x -> t.make (g x));
      cast = (fun context l -> let v, rest = t.cast context l in (f context v, rest))
    }

let wrap_array elt ~make ~cast =
  let typ = type_single elt in
  Ccontainer
    { dtype = Tarray typ;
      make = (let f = make_single elt in
              fun x -> array typ (make f x));
      cast = (let f = _cast_single elt in
              fun context -> function
                | Array(t, l) when t = typ -> cast (f context) l
                | _ -> raise Cast_failure) }

let wrap_dict tyk tyv ~make ~cast =
  let ktyp = type_basic tyk and vtyp = type_single tyv in
  Ccontainer
    { dtype = Tdict(ktyp, vtyp);
      make = (let f = make_basic tyk and g = make_single tyv in
              fun x -> dict ktyp vtyp (make f g x));
      cast = (let f = _cast_basic tyk and g = _cast_single tyv in
              fun context -> function
                | Dict(tk, tv, l) when tk = ktyp && tv = vtyp -> cast (f context) (g context) l
                | _ -> raise Cast_failure) }

let wrap_array_with_context elt ~make ~cast =
  let typ = type_single elt in
  Ccontainer
    { dtype = Tarray typ;
      make = (let f = make_single elt in
              fun x -> array typ (make f x));
      cast = (let f = _cast_single elt in
              fun context -> function
                | Array(t, l) when t = typ -> cast context (f context) l
                | _ -> raise Cast_failure) }

let wrap_dict_with_context tyk tyv ~make ~cast =
  let ktyp = type_basic tyk and vtyp = type_single tyv in
  Ccontainer
    { dtype = Tdict(ktyp, vtyp);
      make = (let f = make_basic tyk and g = make_single tyv in
              fun x -> dict ktyp vtyp (make f g x));
      cast = (let f = _cast_basic tyk and g = _cast_single tyv in
              fun context -> function
                | Dict(tk, tv, l) when tk = ktyp && tv = vtyp -> cast context (f context) (g context) l
                | _ -> raise Cast_failure) }

(* +---------+
   | Helpers |
   +---------+ *)

let rec assocl key = function
  | (key', x) :: l ->
      if key = key' then
        x
      else
        assocl key l
  | [] ->
      raise Cast_failure

let rec assocr key = function
  | (x, key') :: l ->
      if key = key' then
        x
      else
        assocr key l
  | [] ->
      raise Cast_failure

let map t l = wrap t (fun x -> assocr x l) (fun x -> assocl x l)

let bitwise t bits =
  wrap t
    (fun x ->
       List.fold_left (fun acc (v, bit) ->
                         if x land (1 lsl bit) <> 0 then
                           v :: acc
                         else
                           acc) [] bits)
    (fun l ->
       List.fold_left (fun acc v -> acc lor (1 lsl (assocl v bits))) 0 l)

let bitwise32 t bits =
  wrap t
    (fun x ->
       List.fold_left (fun acc (v, bit) ->
                         if Int32.logand x (Int32.shift_left 1l bit) <> 0l then
                           v :: acc
                         else
                           acc) [] bits)
    (fun l ->
       List.fold_left (fun acc v -> Int32.logor acc (Int32.shift_left 1l (assocl v bits))) 0l l)

let bitwise64 t bits =
  wrap t
    (fun x ->
       List.fold_left (fun acc (v, bit) ->
                         if Int64.logand x (Int64.shift_left 1L bit) <> 0L then
                           v :: acc
                         else
                           acc) [] bits)
    (fun l ->
       List.fold_left (fun acc v -> Int64.logor acc (Int64.shift_left 1L (assocl v bits))) 0L l)

(* +------------------+
   | Predefined types |
   +------------------+ *)

module OBus_pervasives =
struct

  let obus_byte = Cbasic
    { dtype = Tbyte;
      make = byte;
      cast = (fun context -> function
                | Byte x -> x
                | _ -> raise Cast_failure) }

  let obus_char = obus_byte

  let obus_int8 = wrap obus_byte
    (fun x -> let x = int_of_char x in
     if x >= 128 then x - 256 else x)
    (fun x -> Char.unsafe_chr (x land 0xff))

  let obus_uint8 = wrap obus_byte
    int_of_char
    (fun x -> Char.unsafe_chr (x land 0xff))

  let obus_boolean = Cbasic
    { dtype = Tboolean;
      make = boolean;
      cast = (fun context -> function
                | Boolean x -> x
                | _ -> raise Cast_failure) }

  let obus_bool = obus_boolean

  let obus_int16 = Cbasic
    { dtype = Tint16;
      make = int16;
      cast = (fun context -> function
                | Int16 x -> x
                | _ -> raise Cast_failure) }

  let obus_int32 = Cbasic
    { dtype = Tint32;
      make = int32;
      cast = (fun context -> function
                | Int32 x -> x
                | _ -> raise Cast_failure) }

  let obus_int = wrap obus_int32 Int32.to_int Int32.of_int

  let obus_int64 = Cbasic
    { dtype = Tint64;
      make = int64;
      cast = (fun context -> function
                | Int64 x -> x
                | _ -> raise Cast_failure) }

  let obus_uint16 = Cbasic
    { dtype = Tuint16;
      make = uint16;
      cast = (fun context -> function
                | Uint16 x -> x
                | _ -> raise Cast_failure) }

  let obus_uint32 = Cbasic
    { dtype = Tuint32;
      make = uint32;
      cast = (fun context -> function
                | Uint32 x -> x
                | _ -> raise Cast_failure) }

  let obus_uint = wrap obus_uint32 Int32.to_int Int32.of_int

  let obus_uint64 = Cbasic
    { dtype = Tuint64;
      make = uint64;
      cast = (fun context -> function
                | Uint64 x -> x
                | _ -> raise Cast_failure) }

  let obus_double = Cbasic
    { dtype = Tdouble;
      make = double;
      cast = (fun context -> function
                | Double x -> x
                | _ -> raise Cast_failure) }

  let obus_float = obus_double

  let obus_string = Cbasic
    { dtype = Tstring;
      make = string;
      cast = (fun context -> function
                | String x -> x
                | _ -> raise Cast_failure) }

  let obus_signature = Cbasic
    { dtype = Tsignature;
      make = signature;
      cast = (fun context -> function
                | Signature x -> x
                | _ -> raise Cast_failure) }

  let obus_object_path = Cbasic
    { dtype = Tobject_path;
      make = object_path;
      cast = (fun context -> function
                | Object_path x -> x
                | _ -> raise Cast_failure) }

  let obus_path = obus_object_path

  let obus_list elt = wrap_array elt List.map List.map

  let obus_byte_array = wrap_array obus_byte
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

  let obus_dict tyk tyv = wrap_dict tyk tyv
    (fun f g l -> List.map (fun (k, v) -> (f k, g v)) l)
    (fun f g l -> List.map (fun (k, v) -> (f k, g v)) l)

  let obus_structure ty = Ccontainer
    { dtype = Tstructure(type_sequence ty);
      make = (let f = make_sequence ty in
              fun x -> structure (f x));
      cast = (let f = _cast_sequence ty in
              fun context -> function
                | Structure l -> f context l
                | _ -> raise Cast_failure) }

  let obus_variant = Ccontainer
    { dtype = Tvariant;
      make = variant;
      cast = (fun context -> function
                | Variant v -> v
                | _ -> raise Cast_failure) }

  let obus_unit = Csequence
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
  type ('a, 'b) dict = ('a * 'b) list
  type 'a structure = 'a
  type variant = OBus_value.single
  type byte_array = string
end

open OBus_pervasives

module type Ordered_single_type = sig
  type t
  val obus_t : (t, _) cl_single
  val compare : t -> t -> int
end

module type Ordered_basic_type = sig
  type t
  val obus_t : (t, _) cl_basic
  val compare : t -> t -> int
end

module Make_set(Ord : Ordered_single_type) =
struct
  include Set.Make(Ord)
  let obus_t = wrap_array Ord.obus_t
    ~make:(fun f set -> fold (fun x acc -> f x :: acc) set [])
    ~cast:(fun f l -> List.fold_left (fun acc x -> add (f x) acc) empty l)
end

module Make_map(Ord : Ordered_basic_type) =
struct
  include Map.Make(Ord)
  let obus_t ty = wrap_dict Ord.obus_t ty
    ~make:(fun f g map -> fold (fun k v acc -> (f k, g v) :: acc) map [])
    ~cast:(fun f g l -> List.fold_left (fun acc (k, v) -> add (f k) (g v) acc) empty l)
end

let (@) a b = Tcons(a, b)
let typ = function
  | Cbasic { dtype = t } -> Tone (Tbasic t)
  | Ccontainer { dtype = t } -> Tone t
  | Csequence { dtype = t } -> t
let make = function
  | Cbasic { make = f } -> (fun v -> Tone (basic(f v)))
  | Ccontainer { make = f } -> (fun v -> Tone (f v))
  | Csequence { make = f } -> f
let cast = function
  | Cbasic { cast = f } ->
      (fun context -> function
         | Basic x :: l -> f context x, l
         | _ -> raise Cast_failure)
  | Ccontainer { cast = f } ->
      (fun context -> function
         | x :: l -> f context x, l
         | _ -> raise Cast_failure)
  | Csequence { cast = f } -> f

let reply ty =
  { ftype = Tnil;
    fmake = (fun acc cont -> cont (tree_get_boundary acc));
    fcast = (fun context -> function
               | [] -> (fun f -> f)
               | _ -> raise Cast_failure);
    freply = (ty : (_, _) cl_sequence :> _ sequence) }

let abstract ty fty =
  { ftype = typ ty @ fty.ftype;
    fmake = (fun acc cont x -> fty.fmake (Tcons(acc, make ty x)) cont);
    fcast = (fun context x ->
               let x, rest = cast ty context x in
               let f = fty.fcast context rest in
               fun g -> f (g x));
    freply = fty.freply }

let (-->) = abstract

let tuple2 t1 t2 = Csequence
  { dtype = typ t1 @ typ t2;
    make = (let make1 = make t1
            and make2 = make t2 in
            fun (x1, x2) ->
              make1 x1
              @ make2 x2);
    cast = (let cast1 = cast t1
            and cast2 = cast t2 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              ((v1, v2), l)) }

let tuple3 t1 t2 t3 = Csequence
  { dtype = typ t1 @ typ t2 @ typ t3;
    make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3 in
            fun (x1, x2, x3) ->
              make1 x1
              @ make2 x2
              @ make3 x3);
    cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              ((v1, v2, v3), l)) }

let tuple4 t1 t2 t3 t4 = Csequence
  { dtype = typ t1 @ typ t2 @ typ t3 @ typ t4;
    make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3
            and make4 = make t4 in
            fun (x1, x2, x3, x4) ->
              make1 x1
              @ make2 x2
              @ make3 x3
              @ make4 x4);
    cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3
            and cast4 = cast t4 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              let v4, l = cast4 context l in
              ((v1, v2, v3, v4), l)) }

let tuple5 t1 t2 t3 t4 t5 = Csequence
  { dtype = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5;
    make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3
            and make4 = make t4
            and make5 = make t5 in
            fun (x1, x2, x3, x4, x5) ->
              make1 x1
              @ make2 x2
              @ make3 x3
              @ make4 x4
              @ make5 x5);
    cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3
            and cast4 = cast t4
            and cast5 = cast t5 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              let v4, l = cast4 context l in
              let v5, l = cast5 context l in
              ((v1, v2, v3, v4, v5), l)) }

let tuple6 t1 t2 t3 t4 t5 t6 = Csequence
  { dtype = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6;
    make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3
            and make4 = make t4
            and make5 = make t5
            and make6 = make t6 in
            fun (x1, x2, x3, x4, x5, x6) ->
              make1 x1
              @ make2 x2
              @ make3 x3
              @ make4 x4
              @ make5 x5
              @ make6 x6);
    cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3
            and cast4 = cast t4
            and cast5 = cast t5
            and cast6 = cast t6 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              let v4, l = cast4 context l in
              let v5, l = cast5 context l in
              let v6, l = cast6 context l in
              ((v1, v2, v3, v4, v5, v6), l)) }

let tuple7 t1 t2 t3 t4 t5 t6 t7 = Csequence
  { dtype = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6 @ typ t7;
    make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3
            and make4 = make t4
            and make5 = make t5
            and make6 = make t6
            and make7 = make t7 in
            fun (x1, x2, x3, x4, x5, x6, x7) ->
              make1 x1
              @ make2 x2
              @ make3 x3
              @ make4 x4
              @ make5 x5
              @ make6 x6
              @ make7 x7);
    cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3
            and cast4 = cast t4
            and cast5 = cast t5
            and cast6 = cast t6
            and cast7 = cast t7 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              let v4, l = cast4 context l in
              let v5, l = cast5 context l in
              let v6, l = cast6 context l in
              let v7, l = cast7 context l in
              ((v1, v2, v3, v4, v5, v6, v7), l)) }

let tuple8 t1 t2 t3 t4 t5 t6 t7 t8 = Csequence
  { dtype = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6 @ typ t7 @ typ t8;
    make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3
            and make4 = make t4
            and make5 = make t5
            and make6 = make t6
            and make7 = make t7
            and make8 = make t8 in
            fun (x1, x2, x3, x4, x5, x6, x7, x8) ->
              make1 x1
              @ make2 x2
              @ make3 x3
              @ make4 x4
              @ make5 x5
              @ make6 x6
              @ make7 x7
              @ make8 x8);
    cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3
            and cast4 = cast t4
            and cast5 = cast t5
            and cast6 = cast t6
            and cast7 = cast t7
            and cast8 = cast t8 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              let v4, l = cast4 context l in
              let v5, l = cast5 context l in
              let v6, l = cast6 context l in
              let v7, l = cast7 context l in
              let v8, l = cast8 context l in
              ((v1, v2, v3, v4, v5, v6, v7, v8), l)) }

let tuple9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = Csequence
  { dtype = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6 @ typ t7 @ typ t8 @ typ t9;
    make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3
            and make4 = make t4
            and make5 = make t5
            and make6 = make t6
            and make7 = make t7
            and make8 = make t8
            and make9 = make t9 in
            fun (x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
              make1 x1
              @ make2 x2
              @ make3 x3
              @ make4 x4
              @ make5 x5
              @ make6 x6
              @ make7 x7
              @ make8 x8
              @ make9 x9);
    cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3
            and cast4 = cast t4
            and cast5 = cast t5
            and cast6 = cast t6
            and cast7 = cast t7
            and cast8 = cast t8
            and cast9 = cast t9 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              let v4, l = cast4 context l in
              let v5, l = cast5 context l in
              let v6, l = cast6 context l in
              let v7, l = cast7 context l in
              let v8, l = cast8 context l in
              let v9, l = cast9 context l in
              ((v1, v2, v3, v4, v5, v6, v7, v8, v9), l)) }

let tuple10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = Csequence
  { dtype = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6 @ typ t7 @ typ t8 @ typ t9 @ typ t10;
    make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3
            and make4 = make t4
            and make5 = make t5
            and make6 = make t6
            and make7 = make t7
            and make8 = make t8
            and make9 = make t9
            and make10 = make t10 in
            fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) ->
              make1 x1
              @ make2 x2
              @ make3 x3
              @ make4 x4
              @ make5 x5
              @ make6 x6
              @ make7 x7
              @ make8 x8
              @ make9 x9
              @ make10 x10);
    cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3
            and cast4 = cast t4
            and cast5 = cast t5
            and cast6 = cast t6
            and cast7 = cast t7
            and cast8 = cast t8
            and cast9 = cast t9
            and cast10 = cast t10 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              let v4, l = cast4 context l in
              let v5, l = cast5 context l in
              let v6, l = cast6 context l in
              let v7, l = cast7 context l in
              let v8, l = cast8 context l in
              let v9, l = cast9 context l in
              let v10, l = cast10 context l in
              ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10), l)) }
