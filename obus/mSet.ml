(*
 * mSet.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a prev =
  | Set of 'a t
  | Node of 'a node
  | Alone

and 'a node = {
  mutable prev : 'a prev;
  mutable next : 'a node option;
  data : 'a;
}

and 'a t = 'a node option ref

let make _ = ref None
let is_empty s = !s = None

let add s x =
  let first = !s in
  let n = { prev = Set s;
            next = first;
            data = x } in
  begin match first with
    | Some f -> f.prev <- Node n
    | None -> ()
  end;
  s := Some n;
  n

let is_alone n = n.prev = Alone

let remove n =
  begin match n.next with
    | None -> ()
    | Some next -> next.prev <- n.prev
  end;
  begin match n.prev with
    | Set s -> s := n.next
    | Node prev -> prev.next <- n.next
    | Alone -> ()
  end;
  n.prev <- Alone;
  n.next <- None

let node x = {
  prev = Alone;
  next = None;
  data = x;
}

let insert n set =
  remove n;
  let first = !set in
  n.next <- first;
  n.prev <- Set set;
  set := Some n;
  match first with
    | Some f -> f.prev <- Node n
    | None -> ()

let iter f s =
  let rec aux = function
    | None -> ()
    | Some n -> f n.data; aux n.next
  in
  aux !s

let fold f acc s =
  let rec aux acc = function
    | None -> acc
    | Some n -> aux (f n.data acc) n.next
  in
  aux acc !s

let filter s x =
  let rec aux x = function
    | None -> Some x
    | Some n -> match n.data x with
        | Some x -> aux x n.next
        | None -> None
  in
  aux x !s

let clear s =
  let rec aux = function
    | None -> ()
    | Some n ->
        let next = n.next in
        n.prev <- Alone;
        n.next <- None;
        aux next
  in
  let first = !s in
  s := None;
  aux first
