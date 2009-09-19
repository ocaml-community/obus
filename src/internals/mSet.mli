(*
 * mSet.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Mutable set of non-comparable objects *)

(** {6 Sets} *)

type 'a t
  (** A set of node *)

val make : unit -> 'a t
  (** Create a new empty set *)

val is_empty : 'a t -> bool

val iter : ('a -> unit) -> 'a t -> unit
  (** Iteration over elements of the set. It is OK to modify the set
      while iterating *)

val fold : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  (** Fold function. Same remark than for [iter] *)

val filter : ('a -> 'a option) t -> 'a -> 'a option
  (** [filter set x] pass [x] though all element of [set] until one
      return [None] *)

val clear : 'a t -> unit
  (** Remove all elements *)

(** {6 Nodes} *)

type 'a node
  (** A single element *)

val node : 'a -> 'a node
  (** Create a new alone node *)

val add : 'a t -> 'a -> 'a node
  (** Create a new node and add it immedialty to the given set *)

val insert : 'a node -> 'a t -> unit
  (** Insert a node at the begining of a set. If the node is already
      in a set then it is first removed then inserted in the new
      set. *)

val remove : 'a node -> unit
  (** Remove an element from its set. Do nothing if it is alone *)

val is_alone : 'a node -> bool
  (** Return weather a node is alone or in a set *)
