(*
 * progress.mli
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Print progression on stdout/stderr *)

type t

val make : string -> int -> t Lwt.t
(** [make prefix max] *)

val incr : t -> unit Lwt.t
(** [incr progress] *)

val close : t -> unit Lwt.t
(** [close progress] *)
