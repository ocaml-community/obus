(*
 * mQueue.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Message queue *)

type 'a t = {
  queued : 'a Queue.t;
  waiters : 'a Lwt.t Queue.t;
}

val create : unit -> 'a t

val put : 'a -> 'a t -> unit
val get : 'a t -> 'a Lwt.t
