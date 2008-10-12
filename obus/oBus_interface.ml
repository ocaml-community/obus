(*
 * oBus_interface.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type name = string

type annotation = name * string
type argument = name option * OBus_value.tsingle

type access = Read | Write | Read_write

type declaration =
  | Method of name * argument list * argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * OBus_value.tsingle * access * annotation list

type t = name * declaration list * annotation list
