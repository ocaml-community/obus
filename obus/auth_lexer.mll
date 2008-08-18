(*
 * auth_lexer.mll
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let eol = " "* "\r"

rule eol = parse
    | " "* "\r" { () }

and data buf = parse
  | ([ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ]) as str {
      Buffer.add_char buf
        (Scanf.sscanf str "%x" char_of_int);
      data buf lexbuf
    }
  | eol { Buffer.contents buf }

and space_and_data = parse
  | " "* as space { if space = "" then (eol lexbuf; "") else
                      let buf = Buffer.create 42 in data buf lexbuf }

and methods = parse
  | [ '\x21'-'\x7f'  ]+ as m { m :: methods lexbuf }
  | [ ' ' ]+ { methods lexbuf }
  | eol { [] }

and space_and_methods = parse
  | " "* as space { if space = "" then (eol lexbuf; []) else methods lexbuf }

and string = parse
  | ([ '\x20'-'\x7f' ]* as str) eol { str }

and space_and_string = parse
  | " "* as space { if space = "" then (eol lexbuf; "") else string lexbuf }

and guid = parse
  | " "+ (([ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ])+ as str) eol
      { str }

and command = parse
  | "REJECTED" { `Rejected(space_and_methods lexbuf) }
  | "OK" { `OK(guid lexbuf) }
  | "DATA" { `Data(space_and_data lexbuf) }
  | "ERROR" { `Error(space_and_string lexbuf) }
