(*
 * tree.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type 'a t = Node of 'a option * (string * 'a t) list

let empty = Node(None, [])

let regexp = Str.regexp "\\."

let rec add name x t =
  let rec aux (Node(i, l)) = function
    | [] -> Node(Some(x), l)
    | n :: rest -> Node(i, aux2 n rest l)
  and aux2 n rest = function
    | [] -> [(n, aux (Node(None, [])) rest)]
    | (s, t) :: l when s = n -> (s, aux t rest) :: l
    | st :: l -> st :: aux2 n rest l
  in
    aux t (Str.split regexp name)

let rec map f (Node(i, l)) =
  Node((match i with
          | None -> None
          | Some(x) -> Some(f x)),
       List.map (fun (s, t) -> (s, map f t)) l)

