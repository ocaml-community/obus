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
  opt_initial_check : code;
  (** Initial size checking instructions *)
  opt_alignment : int;
  (** Guarenteed pointer alignement after execution of the caml
      expression *)
  opt_relative_position : int;
  (** Relative position of the pointer in relation to the alignment
      after execution *)
}

val optimize : int -> int -> code -> optimize_result
  (** [optimize rel_pos alignement code] optimize and compile the
      given abstract code.

      [rel_pos] and [alignement] are the guarenteed alignment and
      relative position at the beginning of code execution *)
