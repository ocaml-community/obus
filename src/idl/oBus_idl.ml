open Lexer
open Lexing
   
exception Parse_failure of string

let parse s =
  let lexbuf = Lexing.from_string s in
  try
    Parser.interfaces Lexer.read lexbuf
  with e ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    raise (Parse_failure (Printf.sprintf "%s: pos [%d] token %s"
                            (Printexc.to_string e)
                            cnum
                            tok))

let parse_file file_name =
  let ic = open_in file_name in
  let lexbuf = Lexing.from_channel ic in
  try
    let ifaces = Parser.interfaces Lexer.read lexbuf in
    close_in ic;
    ifaces
  with e ->
    close_in ic;
    let curr = lexbuf.Lexing.lex_curr_p in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    raise (Parse_failure (Printf.sprintf "%s: pos [%d] token %s"
                            (Printexc.to_string e)
                            cnum
                            tok))

(* +-----------------------------------------------------------------+
   | Printing                                                        |
   +-----------------------------------------------------------------+ *)

open OBus_introspect_ext
open OBus_value
open Format

let rec print_term top pp = function
  | Term(id, []) -> pp_print_string pp id
  | Term(id, [t]) -> fprintf pp "%a %s" (print_term false) t id
  | Term(id, tl) -> fprintf pp "(%a) %s" (print_seq true ", ") tl id
  | Tuple tl -> if top then print_seq false " * " pp tl else fprintf pp "(%a)" (print_seq false " * ") tl

and print_seq top sep pp = function
  | [] -> ()
  | [t] -> print_term top pp t
  | t :: tl -> fprintf pp "%a%s%a" (print_term top) t sep (print_seq top sep) tl

let print_args pp args =
  let rec aux = function
    | [] ->
        ()
    | [(None, typ)] ->
        fprintf pp "_ : %a" (print_term true) typ
    | [(Some name, typ)] ->
        fprintf pp "%s : %a" name (print_term true) typ
    | (None, typ) :: l ->
        fprintf pp "_ : %a, " (print_term true) typ;
        aux l
    | (Some name, typ) :: l ->
        fprintf pp "%s : %a, " name (print_term true) typ;
        aux l
  in
  pp_print_char pp '(';
  aux args;
  pp_print_char pp ')'

let print_annotations pp = function
  | [] ->
      ()
  | l ->
      pp_print_string pp "    with {\n";
      List.iter (fun (name, value) -> fprintf pp "      %s = %S\n" name value) l;
      pp_print_string pp "    }\n"

let string_of_key = function
  | T.Byte -> "byte"
  | T.Int16 -> "int16"
  | T.Int32 -> "int32"
  | T.Int64 -> "int64"
  | T.Uint16 -> "uint16"
  | T.Uint32 -> "uint32"
  | T.Uint64 -> "uint64"
  | _ -> assert false

let print pp interfaces =
  List.iter
    (function (name, members, symbols, annotations) ->
       fprintf pp "\ninterface %s {\n" name;
       List.iter
         (fun (name, sym) ->
            let keyword, typ, values =
              match sym with
                | Sym_enum(typ, values) -> "enum", typ, values
                | Sym_flag(typ, values) -> "flag", typ, values
            in
            fprintf pp "  %s %s : %s {\n" keyword name (string_of_key typ);
            let values =
              List.map
                (fun (key, name) ->
                   ((match key with
                       | V.Byte x ->
                           sprintf "%x" (Char.code x)
                       | V.Int16 x | V.Uint16 x ->
                           sprintf "%x" x
                       | V.Int32 x | V.Uint32 x ->
                           sprintf "%lx" x
                       | V.Int64 x | V.Uint64 x ->
                           sprintf "%Lx" x
                       | _ ->
                           assert false),
                    name))
                values
            in
            let max_len = List.fold_left (fun m (key, name) -> max m (String.length key)) 0 values in
            List.iter
              (fun (key, name) ->
                 fprintf pp "    0x%s%s: %s\n" (String.make (max_len - String.length key) '0') key name)
              values;
            fprintf pp "  }\n")
         symbols;
       List.iter (fun (name, value) -> fprintf pp "  annotation %s = %S\n" name value) annotations;
       List.iter
         (function
            | Method(name, i_args, o_args, annotations) ->
                fprintf pp "  method %s : %a -> %a\n" name print_args i_args print_args o_args;
                print_annotations pp annotations
            | Signal(name, args, annotations) ->
                fprintf pp "  signal %s : %a\n" name print_args args;
                print_annotations pp annotations
            | Property(name, typ, access, annotations) ->
                fprintf pp "  property.%s %s : %a\n"
                  (match access with
                     | Read -> "r"
                     | Write -> "w"
                     | Read_write -> "rw")
                  name (print_term true) typ;
                print_annotations pp annotations)
         members;
       pp_print_string pp "}\n")
    interfaces

let print_file name interfaces =
  let oc = open_out name in
  let pp = formatter_of_out_channel oc in
  try
    print pp interfaces;
    pp_print_flush pp ();
    close_out oc
  with exn ->
    (* Should never happen *)
    close_out oc;
    raise exn
