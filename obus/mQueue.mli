(*
 * mQueue.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Message queue *)

type 'a t

val create : unit -> 'a t

val put : 'a -> 'a t -> unit

val get : 'a t -> 'a Lwt.t
  (** Never fail, return a waiting thread if the queue is empty *)

val clear : 'a t -> unit

val abort : 'a t -> exn -> unit
  (** Abort the queue:

      - clear the queue
      - all future operation will fail with this exception
      - wakeup all waiters with the given exception *)

val aborted : 'a t -> exn option

(** Queue can be up or down, down means that {!get} will always return
    a waiting thread *)

val is_up : 'a t -> bool
val set_up : 'a t -> unit
val set_down : 'a t -> unit
