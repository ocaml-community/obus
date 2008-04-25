(*
 * protected.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type 'a t
  (** A mutable value protected by a mutex *)

val make : 'a -> 'a t
  (** [make v] create a new protected value *)

val set : 'a t -> 'a -> unit
  (** [set p v] change the content of a protected value *)

val get : 'a t -> 'a
  (** [get p] get the content of a protected value *)

val update : ('a -> 'a) -> 'a t -> unit
  (** [update p f] update the content of a protected value, [f] must
      not raise exception *)

val safe_update : ('a -> 'a) -> 'a t -> unit
  (** same as [update] but handle the case when [f] raise exception *)

val process : ('a -> ('b * 'a)) -> 'a t -> 'b
  (** [process p f] same as process but also return a value *)

val safe_process : ('a -> ('b * 'a)) -> 'a t -> 'b
  (** same as [process] but handle the case when [f] raise
      exception *)
