(*
 * obus-binder.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Types
open Introspect

let _loc = Loc.ghost

let output_file_prefix = ref None
let xml_files = ref []
let internal = ref false

let args = [
  "-o", Arg.String (fun s -> output_file_prefix := Some s),
  Printf.sprintf "output file prefix";
  "-internal", Arg.Set internal,
  "generate a module bound to be part of the obus library";
]

let usage_msg = Printf.sprintf "Usage: %s <options> <xml-files>
Generate an ocaml module from DBus introspection xml files.
options are:" (Filename.basename (Sys.argv.(0)))

let choose_output_file_prefix () = match !output_file_prefix with
  | Some f -> f
  | None -> match !xml_files with
      | [f] -> begin
          try
            let i = String.rindex f '.' in
              if String.sub f i (String.length f - i) = ".xml"
              then String.sub f 0 i
              else f
          with
              Not_found -> f
        end
      | _ -> "obus.out"

let _ =
  Arg.parse args
    (fun s -> xml_files := s :: !xml_files)
    usage_msg;

  if !xml_files = []
  then Arg.usage args usage_msg;

  let output_file_prefix = choose_output_file_prefix () in
  let node = parse_xmls (List.map Util.parse_xml !xml_files) in
  let implem = GenImplem.gen GenSerializer.default_rules node
  and interf = GenInterf.gen node in

    Printers.OCaml.print_implem ~output_file:(output_file_prefix ^ ".ml")
      (if !internal
       then (<:str_item<
             open Values;;
             open Wire;;
             $implem$>>)
       else (<:str_item<
             open OBus;;
             open Values;;
             open Wire;;
             $implem$>>));
    Printers.OCaml.print_interf ~output_file:(output_file_prefix ^ ".mli")
      (if !internal
       then interf
       else (<:sig_item<
             open OBus
             $interf$>>))
