(*
 * oBus_match_rule_lexer.mll
 * -------------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

rule match_rules = parse
    | (['a'-'z' '_' '0'-'9']+ as key) "='" ([^ '\'']* as value) '\''
        { if comma lexbuf then
            (key, value) :: match_rules lexbuf
          else if end_of_rules lexbuf then
            [(key, value)]
          else
            raise Exit }

and comma = parse
    | ',' { true }
    | ""  { false }

and end_of_rules = parse
    | eof { true }
    | ""  { false }

and arg = parse
    | "arg" (['0'-'9']+ as nb) { Some(int_of_string nb) }
    | ""                       { None }
