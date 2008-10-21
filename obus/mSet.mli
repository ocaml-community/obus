(*
 * mSet.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Mutable set of non-comparable objects *)

type 'a t

type 'a node
  (** A single element *)

val make : unit -> 'a t
  (** Create a new empty set *)

val is_empty : 'a t -> bool
val add : 'a t -> 'a -> 'a node

val enabled : 'a node -> bool
  (** Tell weather a node is in the set it was originally added *)

val enable : 'a node -> unit
  (** Ensure the node is in its set *)

val disable : 'a node -> unit
  (** Remove the node from its set.

      Note: It is immediatly disabled, i.e. if we are iterating on the
      set, and it has not yet been reached, it will be skipped *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Iteration over elements of the set. It is OK to modify the set
      while iterating *)

val fold : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  (** Fold function. Same remark than for [iter] *)

val clear : 'a t -> unit
  (** Disable all elements *)
