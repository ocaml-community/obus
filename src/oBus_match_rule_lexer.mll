(*
 * oBus_match_rule_lexer.mll
 * -------------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

{
  exception Fail of int * string

  let pos lexbuf = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum

  let fail lexbuf fmt =
    Printf.ksprintf
      (fun msg -> raise (Fail(pos lexbuf, msg)))
      fmt
}

rule match_rules = parse
    | (['a'-'z' '_' '0'-'9']+ as key) "='" ([^ '\'']* as value) '\''
        { if comma lexbuf then
            (pos lexbuf, key, value) :: match_rules lexbuf
          else begin
            check_eof lexbuf;
            [(pos lexbuf, key, value)]
          end }
    | "=" {
        fail lexbuf "empty key"
      }
    | eof {
        fail lexbuf "match rule expected"
      }
    | _ as ch {
        fail lexbuf "invalid character %C" ch
      }

and comma = parse
    | ',' { true }
    | ""  { false }

and check_eof = parse
    | eof { () }
    | _ as ch { fail lexbuf "invalid character %C" ch }

and arg = parse
  | "arg" (['0'-'9']+ as n) (("" | "path" | "namespace") as kind) eof {
        let n = int_of_string n in
        if n >= 0 && n <= 63 then
          Some(n,
               match kind with
                 | "" -> `String
                 | "path" -> `Path
                 | "namespace" -> `Namespace
                 | _ -> assert false)
        else
          fail lexbuf "invalid argument number '%d': it must be between 0 and 63" n
      }
  | "" { None }
