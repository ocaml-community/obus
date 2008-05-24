(*
 * optimize.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types
open AbstractCode

type optimize_result = {
  opt_code : code;
  (** Optimized code *)
  opt_size : int option;
  (** Size of value readen/written if fixed and calculable *)
  opt_without_initial_check : code;
  (** Same as [opt_code] but without the initial instruction for size
      checking *)
  opt_alignment : int;
  (** Guarenteed pointer alignement after execution of the caml
      expression *)
  opt_relative_position : int;
  (** Relative position of the pointer in relation to the alignment
      after execution *)
}

val optimize : bool -> int -> int -> code -> optimize_result
  (** [optimize padding_important rel_pos alignement code] optimize
      and compile the given abstract code.

      [padding_important] tell weather make a difference between real
      Advance instructions and ones generated for padding.

      [rel_pos] and [alignement] are the guarenteed alignment and
      relative position at the beginning of code execution *)
