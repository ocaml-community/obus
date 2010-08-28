(*
 * oBus_top.ml
 * -----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let eval_phrase s =
  let lexbuf = Lexing.from_string s in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase false Format.err_formatter phrase

let _ = eval_phrase "#install_printer OBus_property.print_map;;"

