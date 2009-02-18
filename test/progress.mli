(*
 * progress.mli
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Print progression on stdout/stderr *)

type t

val make : string -> int -> t
  (** [make prefix max] *)

val incr : t -> unit
  (** [incr progress] *)

val close : t -> unit
  (** [close progress] *)
