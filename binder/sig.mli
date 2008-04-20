(*
 * sig.mli
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Abstract representation of an interface *)

type name = string

type 'a param = Arg of name * 'a
  (** A argument of a function/signal *)

type 'a definition =
  | Method of name * 'a param list * 'a param list
  | Signal of name * 'a param list

type 'a t = Sig of name * 'a definition list
  (** Description of an interface, with its name and list of
      method/signal definition *)

type 'a tree = Tree of ('a t * 'a tree) list
  (** A hierarchy of interfaces *)

val empty : 'a tree
  (** [empty] empty interface hierarchy *)

val add : name -> 'a definition list -> 'a tree -> 'a tree
  (** [add name defs] add an interface definition into a hierarchy *)

val merge : 'a tree -> 'a tree -> 'a tree
  (** [merge a b] merge two interfaces hierarchy *)
