(*
 * oBus_type_ext_lexer.mll
 * -----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

{
  open OBus_value

  exception Fail of int * string

  let pos lexbuf = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum

  let fail lexbuf fmt =
    Printf.ksprintf
      (fun msg -> raise (Fail(pos lexbuf, msg)))
      fmt

  type term =
    | Term of string * term list
    | Tuple of term list

  let term name args = Term(name, args)
  let tuple = function
    | [t] -> t
    | l -> Tuple l
}

let int = ['-' '+']? ['0'-'9']+
let space = [' ' '\t' '\n']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule enum_and_flag = parse
  | space* (ident as name) space* ":" (ident as typ) "="
      { let typ = match typ with
          | "byte" -> T.Byte
          | "int16" -> T.Int16
          | "int32" -> T.Int32
          | "int64" -> T.Int64
          | "uint16" -> T.Uint16
          | "uint32" -> T.Uint32
          | "uint64" -> T.Uint64
          | _ -> fail lexbuf "invalid key type: %S" typ
        in
        let values = values typ lexbuf in
        eoi lexbuf;
        (name, typ, values) }
  | ""
      { fail lexbuf "syntax error" }

and eoi = parse
  | space* eof { () }
  | "" { fail lexbuf "syntax error" }

and values typ = parse
  | space* (int as key) space* ":" space* (ident as name)
      {
        let key = match typ with
          | T.Byte -> V.Byte(char_of_int (int_of_string key))
          | T.Int16 -> V.Int16(int_of_string key)
          | T.Int32 -> V.Int32(Int32.of_string key)
          | T.Int64 -> V.Int64(Int64.of_string key)
          | T.Uint16 -> V.Uint16(int_of_string key)
          | T.Uint32 -> V.Uint32(Int32.of_string key)
          | T.Uint64 -> V.Uint64(Int64.of_string key)
          | _ -> assert false
        in
        if comma lexbuf then
          (key, name) :: values typ lexbuf
        else
          [(key, name)]
      }
  | ""
      {
        fail lexbuf "syntax error"
      }

and comma = parse
  | space* "," { true }
  | "" { false }

and single = parse
  | space* (ident as name)
      { term name [] }
  | space* "(" (ident as name)
      { term name (type_args lexbuf) }
  | space* "<"
      { tuple (tuple_args lexbuf) }
  | "" { fail lexbuf "syntax error" }

and type_args = parse
  | space* ")" { [] }
  | "" { let typ = single lexbuf in typ :: type_args lexbuf }

and tuple_args = parse
  | space* ">" { [] }
  | "" { let typ = single lexbuf in typ :: tuple_args2 lexbuf }

and tuple_args2 = parse
  | space* ">" { [] }
  | space* "," { let typ = single lexbuf in typ :: tuple_args2 lexbuf }
  | "" { fail lexbuf "syntax error" }
