(*
 * oBus_cache.mli
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Simple data caching *)

type 'a t

val create : int -> 'a t
  (** [create size] creates a new cache which can contains at most
      [size] element *)

val add : 'a t -> 'a -> unit
  (** [add cache x] adds an element to the cache. Oldest elements may
      be removed if the cache is full *)

val mem : 'a t -> 'a -> bool
  (** [mem cache x] returns [true] iff [x] is in the cache *)

val clear : 'a t -> unit
  (** [clear cache] remove all elements of the cache *)
