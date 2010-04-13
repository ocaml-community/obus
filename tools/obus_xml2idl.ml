(*
 * obus_xml2idl.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let usage_message =
  Printf.sprintf "Usage: %s <options> <file>\n\
                  Generate an obus IDL file from a D-Bus introspection file.\n\
                  options are:"
    (Filename.basename Sys.argv.(0))

let output = ref ""

let args = [
  "-o", Arg.Set_string output, "<file-name> output file name";
]

let () =
  let sources = ref [] in
  Arg.parse args (fun s -> sources := s :: !sources) usage_message;

  let source =
    match !sources with
      | [s] -> s
      | _ -> Arg.usage args usage_message; exit 1
  in
  let destination =
    match !output with
      | "" ->
          (try
             Filename.chop_extension source
           with Invalid_argument _ ->
             source) ^ ".obus"
      | name ->
          name
  in

  Idl.print destination (Utils.IFSet.elements (Utils.parse_xml source));
  Printf.printf "file \"%s\" written\n" destination
