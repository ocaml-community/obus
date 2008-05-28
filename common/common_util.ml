(*
 * common_util.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type S = sig
  val assoc : 'a -> ('a * 'b) list -> 'b option
  val find_map : ('a -> 'b option) -> 'a list -> 'b option
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val try_all : ('a -> 'b option) list -> 'a -> 'b option
  val select : 'a -> ('a -> 'b option) list -> 'b list
  val exn_to_opt : ('a -> 'b) -> 'a -> 'b option
end

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
