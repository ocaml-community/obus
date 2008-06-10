(*
 * compile.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types

type env
  (** The environment represent the list of already generated
      functions. *)

val lookup : expr -> env -> string * env
  (** [lookup expr env] search a function which have expression [expr]
      in [env] and return its associated name.

      If [expr] is not found it is added to [env]. *)

val empty_env : env

val dump_env : env -> (string * expr) list
  (** [dump_env env] return the list of defined function in the
      environment with their associated name *)

val compile_reader : env Instruction.t list -> (expr list -> expr) -> env -> expr * env
val compile_writer : env Instruction.t list -> expr -> env -> ident list * expr * env
  (** Make an expression from a list of instructions. The resulting
      expression is not added to the resulting
      environment. [compile_writer] return also the list of variablies
      that will be written by the expression. *)
