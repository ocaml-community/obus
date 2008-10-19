(*
 * obus-binder.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Term
open Common
open Format
open OBus_interface

let output_file_prefix = ref None
let xml_files = ref []
let no_sugar = ref false
let service_mode = ref false

let args = [
  "-o", Arg.String (fun s -> output_file_prefix := Some s),
  "output file prefix";
  "-service", Arg.Set service_mode,
  "generate code for service implementation instead of proxy code";
  "-no-sugar", Arg.Set no_sugar,
  "disable the use of syntactic sugars in generated files";
  "-name-translator", Arg.Symbol(["upper"; "lower"],
                                 (function
                                    | "upper" -> translator := Upper
                                    | "lower" -> translator := Lower
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

let with_pp fname f = Util.with_open_out fname
  (fun oc ->
     f (Format.formatter_of_out_channel oc);
     Printf.eprintf "File %S written.\n" fname)

module Interf_set = Set.Make(struct type t = OBus_interface.t let compare = compare end)

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

(***** Printing of proxy code *****)

let im_term_of_args = List.map (fun (name, typ) -> implem_term_of_single typ)

let print_proxy_implem_no_sugar pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a = struct\n" puid name;
  p "  include OBus_client.Make(struct let name = %S end)\n" name;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  let %a = call %S %a\n" plid name name
          (print_func_no_sugar (tuple (im_term_of_args  outs)))
          (im_term_of_args ins)
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (im_term_of_args args)
        in
        p "  let on_%a = on_signal %S %a\n" plid name name
          (print_term_no_sugar false) args
    | Property(name, typ, access, annots) ->
        let access = match access with
          | Read -> "rd_only"
          | Write -> "wr_only"
          | Read_write -> "rdwr"
        and term = implem_term_of_single typ in
        p "  let %a = property %S OBus_property.%s %a\n" plid name name access
          (print_term_no_sugar false) term
  end content;
  p "end\n"

let print_proxy_implem_sugar pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a = struct\n" puid name;
  p "  include OBus_client.Make(struct let name = %S end)\n" name;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  OBUS_method %s : %a\n" name
          (print_func (tuple (im_term_of_args  outs)))
          (im_term_of_args ins)
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (im_term_of_args args)
        in
        p "  OBUS_signal %s : %a\n" name
          (print_term true) args
    | Property(name, typ, access, annots) ->
        let access = match access with
          | Read -> "r"
          | Write -> "w"
          | Read_write -> "rw"
        and term = implem_term_of_single typ in
        p "  OBUS_property_%s %s : %a\n" access name
          (print_term true) term
  end content;
  p "end\n"

let str_of_access = function
  | Read -> "r"
  | Write -> "w"
  | Read_write -> "rw"

(***** Printing of service code *****)

let print_service_implem_no_sugar pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "class virtual %a = object(self)\n" plid name;
  p "  inherit OBus_object.interface\n";
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  method virtual %s : %a\n" (String.uncapitalize name)
          (print_func (tuple (if_term_of_args  outs)))
          (if_term_of_args ins)
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (im_term_of_args args)
        in
        p "  method %s = self#obus_emit_signal %S %a\n" (String.uncapitalize name) name
          (print_term_no_sugar false) args
    | Property(name, typ, access, annots) ->
        p "  val virtual mutable %s : %a\n"
          (String.uncapitalize name) (print_term true) (interf_term_of_single typ)
  end content;
  p "  initializer\n";
  p "    self#obus_add_interface %S [\n" name;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "      md_method %S %a (fun _ -> self#%s)\n" name
          (print_func_no_sugar (tuple (im_term_of_args  outs)))
          (im_term_of_args ins)
          (String.uncapitalize name)
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (im_term_of_args args)
        in
        p "      md_signal %S %a\n" name (print_term_no_sugar false) args
    | Property(name, typ, access, annots) ->
        let n = String.uncapitalize name in
        p "      md_property_%s %S " (str_of_access access) name;
        begin match access with
          | Read -> p "(fun _ -> %s)\n" n
          | Write -> p "(fun _%s -> %s <- _%s)\n" n n n
          | Read_write -> p "(fun _ -> %s) (fun _%s -> %s <- _%s)\n" n n n n
        end
  end content;
  p "    ]\n";
  p "end\n"

let print_service_implem_sugar pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "class virtual %a = OBUS_interface %S\n" plid name name;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  OBUS_method %s : %a\n" name
          (print_func (tuple (if_term_of_args  outs)))
          (if_term_of_args ins)
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (if_term_of_args args)
        in
        p "  OBUS_signal %s : %a\n" name
          (print_term true) args
    | Property(name, typ, access, annots) ->
        p "  OBUS_val_%s mutable %s : %a\n" (match access with
                                               | Read -> "r"
                                               | Write -> "w"
                                               | Read_write -> "rw")
          name (print_term true) (interf_term_of_single typ)
  end content;
  p "end\n"

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
      (fun pp -> Interf_set.iter (print_proxy_interf pp) interfaces);

  let printer = match !service_mode, !no_sugar with
    | true, true -> print_service_implem_no_sugar
    | true, false -> print_service_implem_sugar
    | false, true -> print_proxy_implem_no_sugar
    | false, false -> print_proxy_implem_sugar in

  with_pp (output_file_prefix ^ ".ml")
    (fun pp ->
       Format.fprintf pp "open OBus_type\n";
       if !service_mode && !no_sugar then Format.fprintf pp "open OBus_object\n";
       Interf_set.iter (printer pp) interfaces)
