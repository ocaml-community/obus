(*
 * oBus_cache.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type 'a t = int * 'a Queue.t
let create size = (size, Queue.create ())
let clear (size, queue) = Queue.clear queue

let mem (size, queue) name =
  Queue.fold (fun acc x -> acc || x = name) false queue

let add (size, queue) name =
  if mem (size, queue) name then
    ()
  else begin
    if Queue.length queue > size then ignore (Queue.pop queue);
    Queue.push name queue
  end
