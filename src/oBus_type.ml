(*
 * oBus_type.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Log = Lwt_log.Make(struct let section = "obus(type)" end)

open OBus_value
open OBus_private_type

type ('a, 'cl) t = ('a, 'cl) OBus_private_type.t

type 'a basic = ('a, [`Basic]) t
type 'a container = ('a, [`Container]) t
type 'a sequence = ('a, [`Sequence]) t

type ('a, 'cl) cl_basic = ('a, 'cl) t
constraint 'cl = [ `Basic ]
type ('a, 'cl) cl_single = ('a, 'cl) t
constraint 'cl = [< `Basic | `Container ]
type ('a, 'cl) cl_sequence = ('a, 'cl) t
constraint 'cl = [< `Basic | `Container | `Sequence ]

type ('a, 'b, 'c) func = {
  f_type : OBus_value.tsingle tree;
  f_make : OBus_value.single tree -> (OBus_value.sequence -> 'b) -> 'a;
  f_cast : full_context -> OBus_value.sequence -> 'a -> 'b;
  f_reply : 'c sequence;
}

type context = OBus_private_type.context

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

exception Cast_failure = OBus_private_type.Cast_failure

let () =
  Printexc.register_printer
    (function
       | Cast_failure(func, msg) ->
           Some(Printf.sprintf "'%s' failed to cast D-Bus value: %s" func msg)
       | _ ->
           None)

let make_basic = OBus_private_type.make_basic
let make_single = OBus_private_type.make_single
let make_sequence = OBus_private_type.make_sequence

let close_fds map =
  FDMap.iter (fun fd_src fd_dup ->
                try
                  Unix.close fd_dup
                with exn ->
                  ignore (Log.exn exn "failed to close file descriptor")) map

let cast f ?(context=No_context) x =
  let context = { ctx_data = context; ctx_fds = FDMap.empty } in
  try
    f context x
  with exn ->
    (* Close all duplicated file descriptors, that are now
       inaccessible: *)
    close_fds context.ctx_fds;
    raise exn

let cast_basic t = cast (_cast_basic t)
let cast_single t = cast (_cast_single t)
let cast_sequence t = cast (_cast_sequence t)

let opt_cast f ?(context=No_context) x =
  let context = { ctx_data = context; ctx_fds = FDMap.empty } in
  try
    Some(f context x)
  with
    | Cast_failure _ ->
        close_fds context.ctx_fds;
        None
    | exn ->
        close_fds context.ctx_fds;
        raise exn

let opt_cast_basic t = opt_cast (_cast_basic t)
let opt_cast_single t = opt_cast (_cast_single t)
let opt_cast_sequence t = opt_cast (_cast_sequence t)

let make_func { f_make = f } cont = f Tnil cont

let cast_func { f_cast = f } ?(context=No_context) x g =
  let context = { ctx_data = context; ctx_fds = FDMap.empty } in
  match try `OK(f context x) with exn -> `Fail exn with
    | `OK apply ->
        apply g
    | `Fail exn ->
        close_fds context.ctx_fds;
        raise exn

let opt_cast_func { f_cast = f } ?(context=No_context) x g =
  let context = { ctx_data = context; ctx_fds = FDMap.empty } in
  match try `OK(f context x) with exn -> `Fail exn with
    | `OK apply ->
        Some(apply g)
    | `Fail(Cast_failure _) ->
        close_fds context.ctx_fds;
        None
    | `Fail exn ->
        close_fds context.ctx_fds;
        raise exn

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
      b_cast = (fun context x -> f context.ctx_data (t.b_cast context x));
    }
  | Ctype t -> Ctype{
      c_type = t.c_type;
      c_make = (fun x -> t.c_make (g x));
      c_cast = (fun context x -> f context.ctx_data (t.c_cast context x));
    }
  | Stype t -> Stype{
      s_type = t.s_type;
      s_make = (fun x -> t.s_make (g x));
      s_cast = (fun context l ->
                  let v, rest = t.s_cast context l in
                  (f context.ctx_data v, rest))
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
      raise (Cast_failure("OBus_type.assocl", "key not found"))

let rec assocr key = function
  | (x, key') :: l ->
      if key = key' then
        x
      else
        assocr key l
  | [] ->
      raise (Cast_failure("OBus_type.assocr", "key not found"))

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
         | _ -> raise (Cast_failure("OBus_type.cast", signature_mismatch)))
  | Ctype { c_cast = f } ->
      (fun context -> function
         | x :: l -> f context x, l
         | _ -> raise (Cast_failure("OBus_type.cast", signature_mismatch)))
  | Stype { s_cast = f } -> f

let reply ty = {
  f_type = Tnil;
  f_make = (fun acc cont -> cont (tree_get_boundary acc));
  f_cast = (fun context -> function
             | [] -> (fun f -> f)
             | _ -> raise (Cast_failure("OBus_type.reply", signature_mismatch)));
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
