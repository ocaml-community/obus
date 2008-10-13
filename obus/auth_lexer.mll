(*
 * auth_lexer.mll
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

{

open Auth_command

}

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

and server_command = parse
  | "REJECTED" { Server_rejected(space_and_methods lexbuf) }
  | "OK" { Server_ok(OBus_uuid.of_string (guid lexbuf)) }
  | "DATA" { Server_data(space_and_data lexbuf) }
  | "ERROR" { eol lexbuf; Server_error }

and auth_init_args = parse
  | eol { None }
  | " " { Some(space_and_data lexbuf) }

and auth_args = parse
  | [ ' ' ]+ ([ '\x21'-'\x7f'  ]+ as m) { Some(m, auth_init_args lexbuf) }
  | eol { None }

and client_command = parse
  | "AUTH" { Client_auth(auth_args lexbuf) }
  | "CANCEL" { eol lexbuf; Client_cancel }
  | "BEGIN" { eol lexbuf; Client_begin }
  | "DATA" { Client_data(space_and_data lexbuf) }
  | "ERROR" { Client_error(space_and_string lexbuf) }
