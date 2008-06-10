(*
 * util.ml
 * -------
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

let part_map f l =
  List.fold_right (fun x (success, failure) -> match f x with
                     | None -> (success, x :: failure)
                     | Some(v) -> (v :: success, failure)) l ([], [])

let try_finally f close arg =
  let result =
    try
      f arg
    with
      | e ->
          close arg;
          raise e
  in
    close arg;
    result

let with_open_in fname f =
  try_finally f close_in (open_in fname)

let with_open_out fname f =
  try_finally f close_out (open_out fname)
