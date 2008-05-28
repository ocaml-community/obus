(*
 * genCode.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open AbstractCode
open Types

val generate_reader : bool -> bool -> Env.t -> code -> (Env.t -> expr) -> expr
  (** [generate_reader for_array remove_final_adv env code
      return_expr] generate caml expression from abstract code for
      unmarshaling.

      [remove_final_adv] tell weather to keep or not the final [let i =
      ... in ...] *)

val generate_writer : bool -> bool -> Env.t -> code -> (Env.t -> expr) -> expr
  (** [generate_writer for_array remove_final_adv env code
      return_expr] generate caml expression from abstract code for
      marshaling *)
