(*
 * cookie.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Asynchronous recption of messages *)

(** A cookie is used to make an asynchronous method call and retreive
    the reply content later. It is like a [lazy] value. The value of
    the cookie became accessible as the reply is arrived. *)

type 'a t = 'a Connection.cookie
    (** A non retreived message content *)

val get : 'a t -> 'a
  (** [get cookie] get the value associated with a cookie, eventually
      waiting for it. If the reply is an error then it will be raised
      here. *)

val is_ready : 'a t -> bool
  (** [is_ready cookie] return true if the cookie is evaluated (or if
      if is an error) *)

val is_value : 'a t -> bool
  (** Return true if the cookie is ready and is not an error *)

val is_exn : 'a t -> bool
  (** Return true if the cookie is ready and is an error *)

val get_if_ready : 'a t -> 'a option
  (** [get_if_ready cookie] return Some(v) where v is the value of
      [cookie] if it is ready, or [None] if not, and raise an
      exception like [get]. *)
