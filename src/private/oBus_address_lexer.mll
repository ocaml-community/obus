(*
 * oBus_address_lexer.mll
 * ----------------------
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

rule addresses = parse
    | [^ ':' ]+ as name {
        check_colon lexbuf;
        let parameters = parameters lexbuf in
        if semi_colon lexbuf then
          (pos lexbuf, name, parameters) :: addresses lexbuf
        else begin
          check_eof lexbuf;
          [(pos lexbuf, name, parameters)]
        end
      }
    | ":" {
        fail lexbuf "empty transport name"
      }
    | eof {
        fail lexbuf "addresses expected"
      }

and semi_colon = parse
    | ";" { true }
    | "" { false }

and check_eof = parse
    | eof { () }
    | _ as ch { fail lexbuf "invalid character %C" ch }

and check_colon = parse
    | ":" { () }
    | "" { fail lexbuf "colon expected after transport name" }

and parameters = parse
    | eof { [] }
    | "" { parameters_aux lexbuf }

and parameters_aux = parse
    | [^ '=' ]+ as key {
        check_equal lexbuf;
        let value = value (Buffer.create 42) lexbuf in
        if coma lexbuf then
          (key, value) :: parameters_aux lexbuf
        else
          [(key, value)]
      }
    | "=" { fail lexbuf "empty key" }

and coma = parse
    | "," { true }
    | "" { false }

and check_equal = parse
    | "=" { () }
    | "" { fail lexbuf "equal expected after key" }

and value buf = parse
    | [ '0'-'9' 'A'-'Z' 'a'-'z' '_' '-' '/' '.' '\\' ] as ch {
        Buffer.add_char buf ch;
        value buf lexbuf
      }
    | "%" {
        Buffer.add_string buf (unescape lexbuf);
        value buf lexbuf
      }
    | "" {
        Buffer.contents buf
      }

and unescape = parse
    | [ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ] as str
        { OBus_util.hex_decode str }
    | ""
        { failwith "two hexdigits expected after '%'" }
