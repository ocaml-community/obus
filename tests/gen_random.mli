(*
 * gen_random.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Generation of random test data *)

val message : unit -> OBus_message.t
(** Generate a random message *)
