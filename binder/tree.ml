(*
 * tree.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type ('a, 'b) t = Node of 'a option * ('b * ('a, 'b) t) list

let empty = Node(None, [])

let rec insert path x node = match node, path with
  | Node(_, sons), [] -> Node(Some x, sons)
  | Node(y, sons), label :: path -> Node(y, insert_in_sons label path x sons)

and insert_in_sons label path x = function
  | [] -> [(label, insert path x empty)]
  | (label', node) :: sons when label' = label -> (label', insert path x node) :: sons
  | son :: sons -> son :: insert_in_sons label path x sons

let map f node =
  let rec aux path (Node(x, sons)) =
    Node((match x with
            | Some x -> Some (f path x)
            | None -> None),
         List.map (fun (label, node) -> (label, aux (path @ [label]) node)) sons)
  in
    aux [] node

let fold f x node =
  let rec aux acc path (Node(x, sons)) =
    List.fold_left
      (fun acc (label, node) -> aux acc (path @ [label]) node)
      (match x with
         | Some x -> f path x acc
         | None -> acc)
      sons
  in
    aux x [] node

let fold_map f x node =
  let rec aux acc path (Node(x, sons)) =
    let acc, x = match x with
      | Some x ->
          let acc, x = f path x acc in
            (acc, Some x)
      | None -> (acc, None) in
    let acc, sons =
      List.fold_left
        (fun (acc, sons) (label, node) ->
           let acc, new_node = aux acc (path @ [label]) node in
             (acc, (label, new_node) :: sons))
        (acc, [])
        sons in
      (acc, Node(x, List.rev sons))
  in
    aux x [] node

let flat f node =
  let rec aux (Node(x, sons)) =
    f x (List.map (fun (label, node) -> (label, aux node)) sons)
  in
    aux node
