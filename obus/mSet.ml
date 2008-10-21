(*
 * mSet.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a prev =
  | Set of 'a set
  | Node of 'a node
  | Alone

and 'a node = {
  set : 'a set;
  mutable prev : 'a prev;
  mutable next : 'a node option;
  data : 'a;
}

and 'a set = 'a node option ref

type 'a t = 'a set

let make _ = ref None
let is_empty s = !s = None

let add s x =
  let first = !s in
  let n = { set = s;
            prev = Set s;
            next = first;
            data = x } in
  begin match first with
    | Some f -> f.prev <- Node n
    | None -> ()
  end;
  s := Some n;
  n

let enabled n = n.prev = Alone

let disable n =
  begin match n.next with
    | None -> ()
    | Some n -> n.prev <- n.prev
  end;
  begin match n.prev with
    | Set s -> s := n.next
    | Node p -> p.next <- n.next
    | Alone -> ()
  end;
  n.prev <- Alone;
  n.next <- None

let enable n =
  if n.prev = Alone then begin
    let first = !(n.set) in
    n.next <- first;
    n.prev <- Set n.set;
    n.set := Some n;
    match first with
      | Some f -> f.prev <- Node n
      | None -> ()
  end

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
