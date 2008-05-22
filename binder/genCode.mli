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

val generate_reader : bool -> Env.t -> code -> (Env.t -> expr) -> expr
  (** [generate_reader code for_array return_expr] generate caml
      expression from abstract code for unmarshaling *)

val generate_writer : bool -> Env.t -> code -> (Env.t -> expr) -> expr
  (** [generate_writer code for_array return_expr] generate caml
      expression from abstract code for marshaling *)
