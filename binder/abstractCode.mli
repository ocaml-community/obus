(*
 * abstractCode.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Caml.PreCast

type check =
    (** A checking instruction *)
  | Chk_size_dynamic of int
      (** Check that there is more space than the content of the
          length variable plus the given number (1 for string, for the
          null terminating byte, and 0 for array) *)
  | Chk_size_fixed of of
      (** Check that there is more space than that *)
  | Chk_array_size of int * int
      (** [Check_array_size(gap, base_size)] Verify that array size +
          [gap] is a multiple of [base_size] *)

type instruction =
  | Align of int
      (** Align the pointer to the given boundary *)
  | Reset_padding of int * int
      (** Tell the padding optimizer to forgot all infered
          informations about padding *)
  | Advance_fixed of int * bool
      (** Advance the pointer by a fixed number of bytes. The boolean
          tell weather it is for padding or not *)
  | Advance_dynamic of int
      (** Advance the pointer by the number of bytes in the length
          variable plus the given number (for strings and arrays) *)
  | Update_env Env.t -> Env.t
      (** Apply the given function to the current environment *)
  | Expr of bool * Env.t -> Ast.expr -> Ast.expr
      (** Arbitrary expression. The arguments of the function are: the
          current environment and the expression to come
          after. Explicit reading/writing comes here. The boolean flag
          tell weather the expression will access to the pointer *)
  | Branches of Env.t -> Ast.expr * (Ast.patt * instruction list * Ast.expr) list
      (** Continue reading/writing according to the given
          expression *)
  | Check of check
  | Nothing

type code = instruction list
