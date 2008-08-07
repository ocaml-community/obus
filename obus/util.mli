(*
 * util.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** This module contain various functions used by both the library and
    the tools *)

val assoc : 'a -> ('a * 'b) list -> 'b option
  (** Same as List.assoc but return an option *)

val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** [find_map f l] Apply [f] on each element of [l] until it return
      [Some x] and return that result or return [None] *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] apply [f] on each element of [l] and return the
      list ef element for which [f] succeed (i.e. return [Some x]) *)

val part_map : ('a -> 'b option) -> 'a list -> 'b list * 'a list
  (** [part_map f l] apply [f] on each element of [l] and return the
      list of success and the list of failure *)

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

val split : ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
  (** Split a list *)

val with_open_in : string -> (in_channel -> 'a) -> 'a
val with_open_out : string -> (out_channel -> 'a) -> 'a
  (** [with_open_* fname f] open [fname], apply [f] on the channel and
      close it after whatever happen *)

val with_process_in : string -> (in_channel -> 'a) -> 'a
val with_process_out : string -> (out_channel -> 'a) -> 'a
  (** Same thing but for processes *)

module type Monad = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module Maybe : sig
  include Monad with type 'a t = 'a option
  val failwith : string -> 'a t
  val wrap : ('a -> 'b) -> 'a t -> 'b t
  val fold : ('a -> 'a t) -> 'a list -> 'a list t
end

module MaybeT(M : Monad) : sig
  include Monad with type 'a t = 'a option M.t
  val failwith : string -> 'a t
  val wrap : ('a -> 'b) -> 'a t -> 'b t
  val fold : ('a -> 'a t) -> 'a list -> 'a list t
end
