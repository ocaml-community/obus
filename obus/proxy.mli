(*
 * proxy.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Representation of DBus proxies *)

(** A proxy is an object on which live on a different processus, but
    which behave as a native ocaml value.

    Operation on this object are handled by the low-level api. *)

type 'a t
  (** Representation of proxy for a value of type ['a] *)

type name = string
type path = string

val make : Connection.t -> 'a Interface.t -> ?destination:name -> path -> 'a t
  (** [make connection interface destination path] create a proxy with
      interface [interface] and [connection] as backend. If
      [destination] is not [None] then it will be used as destination
      when sending messages *)

val path : 'a t -> path
  (** [path proxy] get the path of a proxy *)

val name : 'a t -> name option
  (** [name proxy] get the connection bus name of [proxy] *)

val connection : 'a t -> Connection.t
  (** [connection proxy] return the connection used for [proxy] *)
