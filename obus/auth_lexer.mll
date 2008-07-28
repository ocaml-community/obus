(*
 * auth_lexer.mll
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

rule eol = parse
    | " "* "\r" { () }

and data buf = parse
  | ([ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ]) as str {
      Buffer.add_char buf
        (Scanf.sscanf str "%x" char_of_int);
      data buf lexbuf
    }
  | " "* "\r" { Buffer.contents buf }

and space_and_data = parse
  | " "* as space { if space = "" then (eol lexbuf; "") else
                      let buf = Buffer.create 42 in data buf lexbuf }

and methods = parse
  | [ '\x21'-'\x7f'  ]+ as m { m :: methods lexbuf }
  | [ ' ' ]+ { methods lexbuf }
  | " "* "\r" { [] }

and space_and_methods = parse
  | " "* as space { if space = "" then (eol lexbuf; []) else methods lexbuf }

and string = parse
  | [ '\x20'-'\x7f' ]* as str { eol lexbuf; str }

and space_and_string = parse
  | " "* as space { if space = "" then (eol lexbuf; "") else string lexbuf }

and command = parse
  | "REJECTED" { `Rejected(space_and_methods lexbuf) }
  | "OK" { `OK(space_and_data lexbuf) }
  | "DATA" { `Data(space_and_data lexbuf) }
  | "ERROR" { `Error(space_and_string lexbuf) }
