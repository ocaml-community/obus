(*
 * optimize.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open AbstractCode

val optimize : int -> int -> code -> code * code * int option * int * int
  (** [optimize rel_pos padding code] optimize [code] by statically
      deducing padding size when possible and factorising checking
      instructions.

      [rel_pos] and [padding] are the guaranted relative position and
      padding at the beginning of code execution.

      results are:
      - the optimized instructions
      - the instructions for initial size checking
      - the size of marshaled values, if constant
      - relative position at the end of code exectuion
      - padding boundary at the end of code execution  *)
