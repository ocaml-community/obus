(*
 * property.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type ('a, 'b) t

val set : 'a Proxy.t -> ('a, 'b) t -> 'b -> unit
  (** [set proxy property value] set a property on an object *)

val get : 'a Proxy.t -> ('a, 'b) t -> 'b
  (** [get proxy property] retreive the value of a property *)

type ('a, 'b) property_list

val get_all : 'a Proxy.t -> ('a, 'b) property_list -> 'b
  (** [get_all proxy properties] all property on an object *)
