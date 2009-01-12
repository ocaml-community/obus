(*
 * addr_lexer.mll
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

rule value addr_acc kv_acc name key buf = parse
  | [ '0'-'9' 'A'-'Z' 'a'-'z' '_' '-' '/' '.' '\\']
      { Buffer.add_char buf (Lexing.lexeme_char lexbuf 0);
        value addr_acc kv_acc name key buf lexbuf;
      }
  | "%" { unescape_char buf lexbuf;
          value addr_acc kv_acc name key buf lexbuf }
  | "," { let v = Buffer.contents buf in
            Buffer.clear buf;
            key_values addr_acc ((key, v) :: kv_acc) name buf lexbuf }
  | ";" { let v = Buffer.contents buf in
            Buffer.clear buf;
            addresses ((name, (key, v) :: kv_acc) :: addr_acc) buf lexbuf }
  | eof { let v = Buffer.contents buf in
            Buffer.clear buf;
            (name, (key, v) :: kv_acc) :: addr_acc }

and unescape_char buf = parse
  | ([ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ]) as str {
      Buffer.add_char buf (Scanf.sscanf str "%x" char_of_int)
    }

and colon = parse
  | ":" { () }

and equal = parse
  | "=" { () }

and key_values addr_acc kv_acc name buf = parse
  | [^ '=' ]* as key {
      equal lexbuf;
      value addr_acc kv_acc name key buf lexbuf
    }

and addresses addr_acc buf = parse
  | eof { addr_acc }
  | [^ ':' ]* as name {
      colon lexbuf;
      key_values addr_acc [] name buf lexbuf;
    }
