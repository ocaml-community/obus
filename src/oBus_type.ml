(*
 * oBus_type.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_value

(* +-----------------------------------------------------------------+
   | Sequence type as tree                                           |
   +-----------------------------------------------------------------+ *)

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

(* +-----------------------------------------------------------------+
   | Type combinators                                                |
   +-----------------------------------------------------------------+ *)

type context = exn
exception No_context

(* Basic type combinator *)
type 'a btype = {
  b_type : tbasic;
  b_make : 'a -> basic;
  b_cast : context -> basic -> 'a;
}

(* Container type combinator *)
type 'a ctype = {
  c_type : tsingle;
  c_make : 'a -> single;
  c_cast : context -> single -> 'a;
}

(* Sequence type combinator *)
type 'a stype = {
  s_type : tsingle tree;
  s_make : 'a -> single tree;
  s_cast : context -> sequence -> 'a * sequence;
}

type ('a, 'cl) t =
  | Btype of 'a btype
  | Ctype of 'a ctype
  | Stype of 'a stype

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
  f_type : OBus_value.tsingle tree;
  f_make : OBus_value.single tree -> (OBus_value.sequence -> 'b) -> 'a;
  f_cast : context -> OBus_value.sequence -> 'a -> 'b;
  f_reply : 'c sequence;
}

let type_basic = function
  | Btype { b_type = t } -> t
  | _ -> assert false
let type_single = function
  | Btype { b_type = t } -> Tbasic t
  | Ctype { c_type = t } -> t
  | _ -> assert false
let type_sequence = function
  | Btype { b_type = t } -> [Tbasic t]
  | Ctype { c_type = t } -> [t]
  | Stype { s_type = t } -> tree_get_boundary t

let isignature { f_type = s } = tree_get_boundary s
let osignature { f_reply = r } = type_sequence r

let make_basic = function
  | Btype { b_make = f } -> f
  | _ -> assert false
let make_single = function
  | Btype { b_make = f } -> (fun x -> basic(f x))
  | Ctype { c_make = f } -> f
  | _ -> assert false
let make_sequence = function
  | Btype { b_make = f } -> (fun x -> [basic(f x)])
  | Ctype { c_make = f } -> (fun x -> [f x])
  | Stype { s_make = f } -> (fun x -> tree_get_boundary (f x))

exception Cast_failure

let _cast_basic = function
  | Btype { b_cast = f } -> f
  | _ -> assert false
let _cast_single = function
  | Btype { b_cast = f } ->
      (fun context -> function
         | Basic x -> f context x
         | _ -> raise Cast_failure)
  | Ctype { c_cast = f } -> f
  | _ -> assert false
let _cast_sequence = function
  | Btype { b_cast = f } ->
      (fun context -> function
         | [Basic x] -> f context x
         | _ -> raise Cast_failure)
  | Ctype { c_cast = f } ->
      (fun context -> function
         | [x] -> f context x
         | _ -> raise Cast_failure)
  | Stype { s_cast = f } ->
      (fun context -> fun l -> match f context l with
         | v, [] -> v
         | _ -> raise Cast_failure)

let cast_basic t ?(context=No_context) x = _cast_basic t context x
let cast_single t ?(context=No_context) x = _cast_single t context x
let cast_sequence t ?(context=No_context) x = _cast_sequence t context x

let opt_cast f ?(context=No_context) x = try Some(f context x) with Cast_failure -> None
let opt_cast_basic t = opt_cast (_cast_basic t)
let opt_cast_single t = opt_cast (_cast_single t)
let opt_cast_sequence t = opt_cast (_cast_sequence t)

let make_func { f_make = f } cont = f Tnil cont
let cast_func { f_cast = f } ?(context=No_context) x g = f context x g
let opt_cast_func { f_cast = f } ?(context=No_context) x g =
  try
    Some(f context x g)
  with
      Cast_failure -> None

let func_reply { f_reply = r } = r

let map t f g = match t with
  | Btype t -> Btype{
      b_type = t.b_type;
      b_make = (fun x -> t.b_make (g x));
      b_cast = (fun context x -> f (t.b_cast context x));
    }
  | Ctype t -> Ctype{
      c_type = t.c_type;
      c_make = (fun x -> t.c_make (g x));
      c_cast = (fun context x -> f (t.c_cast context x));
    }
  | Stype t -> Stype{
      s_type = t.s_type;
      s_make = (fun x -> t.s_make (g x));
      s_cast = (fun context l -> let v, rest = t.s_cast context l in (f v, rest))
    }

let map_with_context t f g = match t with
  | Btype t -> Btype{
      b_type = t.b_type;
      b_make = (fun x -> t.b_make (g x));
      b_cast = (fun context x -> f context (t.b_cast context x));
    }
  | Ctype t -> Ctype{
      c_type = t.c_type;
      c_make = (fun x -> t.c_make (g x));
      c_cast = (fun context x -> f context (t.c_cast context x));
    }
  | Stype t -> Stype{
      s_type = t.s_type;
      s_make = (fun x -> t.s_make (g x));
      s_cast = (fun context l -> let v, rest = t.s_cast context l in (f context v, rest))
    }

(* +-----------------------------------------------------------------+
   | Helpers                                                         |
   +-----------------------------------------------------------------+ *)

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

let mapping t l = map t (fun x -> assocr x l) (fun x -> assocl x l)

let bitwise t bits =
  map t
    (fun x ->
       List.fold_left (fun acc (v, bit) ->
                         if x land (1 lsl bit) <> 0 then
                           v :: acc
                         else
                           acc) [] bits)
    (fun l ->
       List.fold_left (fun acc v -> acc lor (1 lsl (assocl v bits))) 0 l)

let bitwise32 t bits =
  map t
    (fun x ->
       List.fold_left (fun acc (v, bit) ->
                         if Int32.logand x (Int32.shift_left 1l bit) <> 0l then
                           v :: acc
                         else
                           acc) [] bits)
    (fun l ->
       List.fold_left (fun acc v -> Int32.logor acc (Int32.shift_left 1l (assocl v bits))) 0l l)

let bitwise64 t bits =
  map t
    (fun x ->
       List.fold_left (fun acc (v, bit) ->
                         if Int64.logand x (Int64.shift_left 1L bit) <> 0L then
                           v :: acc
                         else
                           acc) [] bits)
    (fun l ->
       List.fold_left (fun acc v -> Int64.logor acc (Int64.shift_left 1L (assocl v bits))) 0L l)

(* +-----------------------------------------------------------------+
   | Predefined types                                                |
   +-----------------------------------------------------------------+ *)

module Perv =
struct

  let obus_byte = Btype {
    b_type = Tbyte;
    b_make = byte;
    b_cast = (fun context -> function
                | Byte x -> x
                | _ -> raise Cast_failure);
  }

  let obus_char = obus_byte

  let obus_int8 = map obus_byte
    (fun x -> let x = int_of_char x in
     if x >= 128 then x - 256 else x)
    (fun x -> Char.unsafe_chr (x land 0xff))

  let obus_uint8 = map obus_byte
    int_of_char
    (fun x -> Char.unsafe_chr (x land 0xff))

  let obus_boolean = Btype {
    b_type = Tboolean;
    b_make = boolean;
    b_cast = (fun context -> function
                | Boolean x -> x
                | _ -> raise Cast_failure);
  }

  let obus_bool = obus_boolean

  let obus_int16 = Btype {
    b_type = Tint16;
    b_make = int16;
    b_cast = (fun context -> function
                | Int16 x -> x
                | _ -> raise Cast_failure);
  }

  let obus_int32 = Btype {
    b_type = Tint32;
    b_make = int32;
    b_cast = (fun context -> function
                | Int32 x -> x
                | _ -> raise Cast_failure);
  }

  let obus_int = map obus_int32 Int32.to_int Int32.of_int

  let obus_int64 = Btype {
    b_type = Tint64;
    b_make = int64;
    b_cast = (fun context -> function
                | Int64 x -> x
                | _ -> raise Cast_failure);
  }

  let obus_uint16 = Btype {
    b_type = Tuint16;
    b_make = uint16;
    b_cast = (fun context -> function
                | Uint16 x -> x
                | _ -> raise Cast_failure);
  }

  let obus_uint32 = Btype {
    b_type = Tuint32;
    b_make = uint32;
    b_cast = (fun context -> function
                | Uint32 x -> x
                | _ -> raise Cast_failure);
  }

  let obus_uint = map obus_uint32 Int32.to_int Int32.of_int

  let obus_uint64 = Btype {
    b_type = Tuint64;
    b_make = uint64;
    b_cast = (fun context -> function
                | Uint64 x -> x
                | _ -> raise Cast_failure);
  }

  let obus_double = Btype {
    b_type = Tdouble;
    b_make = double;
    b_cast = (fun context -> function
                | Double x -> x
                | _ -> raise Cast_failure);
  }

  let obus_float = obus_double

  let obus_string = Btype {
    b_type = Tstring;
    b_make = string;
    b_cast = (fun context -> function
                | String x -> x
                | _ -> raise Cast_failure);
  }

  let obus_signature = Btype {
    b_type = Tsignature;
    b_make = signature;
    b_cast = (fun context -> function
                | Signature x -> x
                | _ -> raise Cast_failure);
  }

  let obus_object_path = Btype {
    b_type = Tobject_path;
    b_make = object_path;
    b_cast = (fun context -> function
                | Object_path x -> x
                | _ -> raise Cast_failure);
  }

  let obus_path = obus_object_path

  let obus_list elt =
    let typ = type_single elt
    and make = make_single elt
    and cast = _cast_single elt in
    Ctype {
      c_type = Tarray typ;
      c_make = (fun l -> array typ (List.map make l));
      c_cast = (fun context -> function
                  | Array(typ', l) when typ' = typ ->
                      List.map (fun x -> cast context x) l
                  | Byte_array s when typ = Tbasic Tbyte ->
                      let rec aux acc = function
                        | -1 -> acc
                        | i -> aux (cast context (sbyte (String.unsafe_get s i)) :: acc) (i - 1)
                      in
                      aux [] (String.length s - 1)
                  | _ -> raise Cast_failure)
    }

  let obus_byte_array = Ctype {
    c_type = Tarray(Tbasic Tbyte);
    c_make = (fun str -> byte_array str);
    c_cast = (fun context -> function
                | Byte_array s -> s
                | _ -> raise Cast_failure);
  }

  let obus_dict tyk tyv =
    let typk = type_basic tyk
    and typv = type_single tyv
    and makek = make_basic tyk
    and makev = make_single tyv
    and castk = _cast_basic tyk
    and castv = _cast_single tyv in
    Ctype {
      c_type = Tdict(typk,  typv);
      c_make = (fun l -> dict typk typv (List.map (fun (k, v) -> (makek k, makev v)) l));
      c_cast = (fun context -> function
                  | Dict(typk', typv', l) when typk' = typk && typv' = typv ->
                      List.map (fun (k, v) -> (castk context k, castv context v)) l
                  | _ ->
                      raise Cast_failure)
    }

  let obus_structure ty = Ctype {
    c_type = Tstructure(type_sequence ty);
    c_make = (let f = make_sequence ty in
              fun x -> structure (f x));
    c_cast = (let f = _cast_sequence ty in
              fun context -> function
                | Structure l -> f context l
                | _ -> raise Cast_failure);
  }

  let obus_variant = Ctype {
    c_type = Tvariant;
    c_make = variant;
    c_cast = (fun context -> function
                | Variant v -> v
                | _ -> raise Cast_failure);
  }

  let obus_unit = Stype {
    s_type = Tnil;
    s_make = (fun () -> Tnil);
    s_cast = (fun context l -> ((), l));
  }

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

let (@) a b = Tcons(a, b)
let typ = function
  | Btype { b_type = t } -> Tone (Tbasic t)
  | Ctype { c_type = t } -> Tone t
  | Stype { s_type = t } -> t
let make = function
  | Btype { b_make = f } -> (fun v -> Tone (basic(f v)))
  | Ctype { c_make = f } -> (fun v -> Tone (f v))
  | Stype { s_make = f } -> f
let cast = function
  | Btype { b_cast = f } ->
      (fun context -> function
         | Basic x :: l -> f context x, l
         | _ -> raise Cast_failure)
  | Ctype { c_cast = f } ->
      (fun context -> function
         | x :: l -> f context x, l
         | _ -> raise Cast_failure)
  | Stype { s_cast = f } -> f

let reply ty = {
  f_type = Tnil;
  f_make = (fun acc cont -> cont (tree_get_boundary acc));
  f_cast = (fun context -> function
             | [] -> (fun f -> f)
             | _ -> raise Cast_failure);
  f_reply = (ty : (_, _) cl_sequence :> _ sequence);
}

let abstract ty fty = {
  f_type = typ ty @ fty.f_type;
  f_make = (fun acc cont x -> fty.f_make (Tcons(acc, make ty x)) cont);
  f_cast = (fun context x ->
             let x, rest = cast ty context x in
             let f = fty.f_cast context rest in
             fun g -> f (g x));
  f_reply = fty.f_reply;
}

let (-->) = abstract

let tuple2 t1 t2 = Stype {
  s_type = typ t1 @ typ t2;
  s_make = (let make1 = make t1
            and make2 = make t2 in
            fun (x1, x2) ->
              make1 x1
              @ make2 x2);
  s_cast = (let cast1 = cast t1
            and cast2 = cast t2 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              ((v1, v2), l));
}

let tuple3 t1 t2 t3 = Stype {
  s_type = typ t1 @ typ t2 @ typ t3;
  s_make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3 in
            fun (x1, x2, x3) ->
              make1 x1
              @ make2 x2
              @ make3 x3);
  s_cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              ((v1, v2, v3), l));
}

let tuple4 t1 t2 t3 t4 = Stype {
  s_type = typ t1 @ typ t2 @ typ t3 @ typ t4;
  s_make = (let make1 = make t1
            and make2 = make t2
            and make3 = make t3
            and make4 = make t4 in
            fun (x1, x2, x3, x4) ->
              make1 x1
              @ make2 x2
              @ make3 x3
              @ make4 x4);
  s_cast = (let cast1 = cast t1
            and cast2 = cast t2
            and cast3 = cast t3
            and cast4 = cast t4 in
            fun context l ->
              let v1, l = cast1 context l in
              let v2, l = cast2 context l in
              let v3, l = cast3 context l in
              let v4, l = cast4 context l in
              ((v1, v2, v3, v4), l));
}

let tuple5 t1 t2 t3 t4 t5 = Stype {
  s_type = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5;
  s_make = (let make1 = make t1
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
  s_cast = (let cast1 = cast t1
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
              ((v1, v2, v3, v4, v5), l));
}

let tuple6 t1 t2 t3 t4 t5 t6 = Stype {
  s_type = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6;
  s_make = (let make1 = make t1
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
  s_cast = (let cast1 = cast t1
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
              ((v1, v2, v3, v4, v5, v6), l));
}

let tuple7 t1 t2 t3 t4 t5 t6 t7 = Stype {
  s_type = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6 @ typ t7;
  s_make = (let make1 = make t1
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
  s_cast = (let cast1 = cast t1
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
              ((v1, v2, v3, v4, v5, v6, v7), l));
}

let tuple8 t1 t2 t3 t4 t5 t6 t7 t8 = Stype {
  s_type = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6 @ typ t7 @ typ t8;
  s_make = (let make1 = make t1
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
  s_cast = (let cast1 = cast t1
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
              ((v1, v2, v3, v4, v5, v6, v7, v8), l));
}

let tuple9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = Stype {
  s_type = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6 @ typ t7 @ typ t8 @ typ t9;
  s_make = (let make1 = make t1
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
  s_cast = (let cast1 = cast t1
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
              ((v1, v2, v3, v4, v5, v6, v7, v8, v9), l));
}

let tuple10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = Stype {
  s_type = typ t1 @ typ t2 @ typ t3 @ typ t4 @ typ t5 @ typ t6 @ typ t7 @ typ t8 @ typ t9 @ typ t10;
  s_make = (let make1 = make t1
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
  s_cast = (let cast1 = cast t1
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
              ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10), l));
}
