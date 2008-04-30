(*
 * common_util_implem.ml
 * ---------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let rec assoc x = function
  | [] -> None
  | (k, v) :: _ when k = x -> Some(v)
  | _ :: l -> assoc x l

let rec find_map f = function
  | [] -> None
  | x :: l -> match f x with
      | None -> find_map f l
      | y -> y

let filter_map f l =
  List.fold_right (fun x acc -> match f x with
                    | None -> acc
                    | Some(v) -> v :: acc) l []

let rec try_all l x = match l with
  | [] -> None
  | f :: l -> match f x with
      | None -> try_all l x
      | y -> y

let rec select x = function
  | [] -> []
  | f :: l -> match f x with
      | None -> select x l
      | Some v -> v :: select x l

let exn_to_opt f x = try Some(f x) with _ -> None
