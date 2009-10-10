(*
 * obus_binder.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Term
open Format
open OBus_introspect

let output_file_prefix = ref None
let xml_files = ref []
let service_mode = ref false

let args = [
  "-o", Arg.String (fun s -> output_file_prefix := Some s),
  "output file prefix";
  "-service", Arg.Set service_mode,
  "generate code for service implementation instead of proxy code";
  "-name-translator", Arg.Symbol(["ocaml"; "haskell"],
                                 (function
                                    | "ocaml" -> Print.translator := `ocaml
                                    | "haskell" -> Print.translator := `haskell
                                    | n -> failwith "invalid name translator")),
  " how to translate dbus names to caml names";
]

let usage_msg = Printf.sprintf "Usage: %s <options> <xml-files>
Generate an ocaml module from DBus introspection files.
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

let with_pp fname =
  Unix.handle_unix_error begin fun f ->
    let oc = open_out fname in
    f (Format.formatter_of_out_channel oc);
    close_out oc;
    Printf.eprintf "File %S written.\n" fname
  end

module Interf_set = Set.Make(struct type t = interface let compare = compare end)

(* Parse an xml file and handle possible errors *)
let parse_file fname =
  let p = XmlParser.make () in
  XmlParser.prove p false;
  try
    OBus_introspect.of_xml (XmlParser.parse p (XmlParser.SFile fname))
  with
    | Xml.Error err ->
        Printf.eprintf "%s: %s\n%!" fname (Xml.error err);
        exit 1
    | OBus_introspect.Parse_failure err ->
        Printf.eprintf "%s is an invalid introspection document.\n" fname;
        OBus_introspect.print_error Format.err_formatter err;
        exit 1

(***** Entry point *****)

let _ =
  Arg.parse args
    (fun s -> xml_files := s :: !xml_files)
    usage_msg;

  if !xml_files = []
  then (Arg.usage args usage_msg; exit 1);

  let output_file_prefix = choose_output_file_prefix () in

  let interfaces = List.fold_left
    (fun acc name ->
       (List.fold_left
          (fun acc interface -> Interf_set.add interface acc)
          acc
          (fst (parse_file name))))
    Interf_set.empty !xml_files in

  if not !service_mode then
    with_pp (output_file_prefix ^ ".mli")
      (fun pp -> Interf_set.iter (Print.print_proxy_interf pp) interfaces);

  with_pp (output_file_prefix ^ ".ml")
    (fun pp ->
       pp_print_string pp "open OBus_type.Perv\n";
       if !service_mode then
         pp_print_string pp "
type t = {
  obus : OBus_object.t;
}

module M = OBus_object.Make(struct
                              type obj = t
                              let get x = x.obus
                            end)

";
       Interf_set.iter (if !service_mode then
                          Print.print_service_implem pp
                        else
                          Print.print_proxy_implem pp) interfaces)
