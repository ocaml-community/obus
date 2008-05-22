(*
 * abstractCode.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types

type instruction =
  | Check_size_dynamic of int
      (** Check that there is more space than the content of the
          length variable plus the given number (1 for string, for
          the null terminating byte, and 0 for array) *)
  | Check_size_fixed of int
      (** Check that there is more space than that *)
  | Check_array_size of int * int
      (** [Check_array_size(gap, base_size)] Verify that array size
          + [gap] is a multiple of [base_size] *)
  | Expr of bool * (Env.t -> expr -> expr)
      (** [Expr(modify_pointer, make_expr) Make an arbitrary
          expression.

          [modify_pointer] tell weather the resulting expression
          will access/change the pointer.

          [make_expr] take the current environment and the
          expression constructed from the next instructions as
          argument. *)
  | Branches of (Env.t -> expr) * ((Env.t -> patt) * instruction list * (Env.t -> expr)) list
      (** Continue reading/writing according to the given
          expression *)
  | Align of int
      (** Align the pointer to the given boundary *)
  | Reset_padding of int * int
      (** Forgot everything about padding *)
  | Advance_fixed of int * bool
      (** Advance the pointer by a fixed number of bytes. The
          boolean tell weather it is for padding or not *)
  | Advance_dynamic of int
      (** Advance the pointer by the number of bytes in the length
          variable plus the given number (for strings and arrays) *)
  | Update_env of (Env.t -> Env.t)
      (** Apply the given function to the current environment *)
  | Nothing

type code = instruction list
