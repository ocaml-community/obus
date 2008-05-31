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
              if String.lowercase (Str.string_after f i) = ".xml"
              then String.sub f 0 i
              else f
          with
              Not_found -> f
        end
      | _ -> "obus.out"

let open_print_header fname =
  let oc = open_out fname in
  let fname = Filename.basename fname in
    Printf.fprintf oc "\
(*
 * %s
 * %s
 *
 * File generated with obus-binder.
 *)
" fname (String.make (String.length fname) '-');
    oc

module Printer = Camlp4.Printers.OCaml.Make(Syntax)

let _ =
  Arg.parse args
    (fun s -> xml_files := s :: !xml_files)
    usage_msg;

  if !xml_files = []
  then Arg.usage args usage_msg;

  let output_file_prefix = choose_output_file_prefix () in
  let node = parse_files !xml_files in
  let implem = GenImplem.gen !internal GenSerializer.default_rules node in

  let implem_fname = output_file_prefix ^ ".ml"
  and interf_fname = output_file_prefix ^ ".mli" in

  let printer = new Printer.printer () in
  let oc = open_print_header implem_fname in

    printer#str_item (Format.formatter_of_out_channel oc)
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

    let oc = open_print_header interf_fname in
      if not !internal
      then output_string oc "\nopen OBus\n";
      PrintInterf.print !internal oc node
