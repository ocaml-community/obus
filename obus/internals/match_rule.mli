(*
 * match_rule.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Matching rules *)

type t
  with obus(basic)

val make :
  ?typ:[ `signal | `error | `method_call | `method_return ] ->
  ?sender:OBus_name.bus ->
  ?interface:OBus_name.interface ->
  ?member:OBus_name.member ->
  ?path:OBus_path.t ->
  ?destination:OBus_name.bus ->
  ?args:(int * string) list ->
  unit -> t
