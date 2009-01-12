(*
 * mQueue.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

type 'a t = {
  queued : 'a Queue.t;
  waiters : 'a Lwt.t Queue.t;
}

let create () = { queued = Queue.create (); waiters = Queue.create () }

let put x q = match Queue.is_empty q.waiters with
  | true -> Queue.push x q.queued
  | false -> wakeup (Queue.pop q.waiters) x

let get q = match Queue.is_empty q.queued with
  | true ->
      let w = wait () in
      Queue.push w q.waiters;
      w
  | false ->
      return (Queue.pop q.queued)
