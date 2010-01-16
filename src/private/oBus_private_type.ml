(*
 * oBus_private_type.ml
 * --------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
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

exception Cast_failure of string * string
let signature_mismatch = "signature mismatch"

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

(* +-----------------------------------------------------------------+
   | Helpers for primitives (OBus_pervasives)                        |
   +-----------------------------------------------------------------+ *)

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

let _cast_basic = function
  | Btype { b_cast = f } -> f
  | _ -> assert false
let _cast_single = function
  | Btype { b_cast = f } ->
      (fun context -> function
         | Basic x -> f context x
         | _ -> raise (Cast_failure("OBus_type.cast_single", signature_mismatch)))
  | Ctype { c_cast = f } -> f
  | _ -> assert false
let _cast_sequence = function
  | Btype { b_cast = f } ->
      (fun context -> function
         | [Basic x] -> f context x
         | _ -> raise (Cast_failure("OBus_type.cast_sequence", signature_mismatch)))
  | Ctype { c_cast = f } ->
      (fun context -> function
         | [x] -> f context x
         | _ -> raise (Cast_failure("OBus_type.cast_sequence", signature_mismatch)))
  | Stype { s_cast = f } ->
      (fun context -> fun l -> match f context l with
         | v, [] -> v
         | _ -> raise (Cast_failure("OBus_type.cast_sequence", signature_mismatch)))

let obus_string = Btype {
  b_type = Tstring;
  b_make = string;
  b_cast = (fun context -> function
              | String x -> x
              | _ -> raise (Cast_failure("OBus_pervasives.obus_string", signature_mismatch)));
}
