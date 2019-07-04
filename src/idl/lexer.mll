{
  open Lexing
  open Parser

  exception SyntaxError of string

}

let lident = ['a'-'z']['a'-'z''0'-'9''_']*

let uident = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*

let integer = ('0'['b''o''x''u'])?['0'-'9']+
  
rule read =
  parse
  | [' ' '\t' '\n']+ { read lexbuf }
  | "interface" { INTERFACE }
  | "method" { METHOD }
  | "signal" { SIGNAL }
  | "property_r" { PROPERTY_R }
  | "property_w" { PROPERTY_W }
  | "property_rw" { PROPERTY_RW }
  | "annotation" { ANNOTATION }
  | "enum" { ENUM }
  | "flag" { FLAG }
  | "with" { WITH }
  | '"' { read_string (Buffer.create 20) lexbuf }
  | "(*" { skip_comment lexbuf }
  | "," { COMMA }
  | "." { PERIOD }
  | "=" { EQMARK }
  | ":" { COLON }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "->" { ARROW }
  | "_" { UNDERSCORE }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | integer as i { INT i }
  | lident as s { LIDENT s }
  | uident as s { UIDENT s }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and skip_comment =
  parse
  | "*)" { read lexbuf }
  | _ { skip_comment lexbuf }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
