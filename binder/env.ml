(*
 * env.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type t = int

let var_id n = (<:ident< $lid:"v" ^ string_of_int n$ >>)
let var_ids n count = List.map var_id (Util.gen_list ((+) 1) n count)

let empty = 0
let init n = n
let size env = env
let add n env = env + n
let nth n env = var_id (env - 1 - n)
let last = nth 0
let lasts n env = var_ids (env - n) n
let slice n m env = var_ids (env - n) m
