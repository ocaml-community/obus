(*
 * constant.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let max_type_recursion_depth = 32
let max_name_length = 255
let max_array_size = 1 lsl 26
let max_message_size = 1 lsl 27
