(*
 * instruction.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Describe the intermediate instruction that are used to generated
    the expression of a reading/writing function *)

open Btypes

type 'env t =
  | Istructure
      (** Mark the beginning of reading/writing a structure, only used
          for padding *)
  | Iarray of ('env -> expr * 'env) * dbus_single_type
      (** [Iarray(mkfunc, elt_type)] read write an array with
          [mkfunc]. [elt_type] is the type of the element of the
          array. This is needed for the initial padding. *)
  | Iaction of string
      (** Read or write some with one of the writer in [Wire] or
          [Values]. *)
  | Iconvert of expr
      (** [Iconvert(func)] convert the last read value or the next
          value to write with [func] *)
  | Ipack of (patt list -> patt) * (expr list -> expr) * 'env t list
      (** [Ipack(mkpatt, mkexpr, instrs)] while reading, pack all the
          values read by [instrs] into one with [mkexpr] and while
          writing unpack the next value to write with [mkpatt]. *)
