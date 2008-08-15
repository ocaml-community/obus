(*
 * common.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Format

module XParser = OBus_xml_parser.Make(struct
                                       type t = Xml.xml
                                       let match_node ~element ~pcdata = function
                                         | Xml.Element(x, y, z) -> element x y z
                                         | Xml.PCData x -> pcdata x
                                     end)

open OBus_introspect

module IParser = Make_parser(XParser)

let source_xml_parser = XmlParser.make ()
let _ = XmlParser.prove source_xml_parser false

let parse_xml xml_parser xml =
  try
    XParser.parse xml_parser xml
  with
      OBus_xml_parser.Parse_failure(stack, err) ->
        eprintf "error encountered while parsing introspection document:\n";
        if stack <> [] then eprintf "in the folowing element:\n";
        OBus_xml_parser.print_stack err_formatter stack;
        eprintf "%s\n%!" err;
        exit 2

let parse_source xml_parser source =
  try
    parse_xml xml_parser (XmlParser.parse source_xml_parser source)
  with
      Xml.Error err ->
        eprintf "error while parsing xml: %s\n%!" (Xml.error err);
        exit 2

(***** Name camlization *****)

(* Apply the following transformations:

   "SetCPUFreqGovernor" -> ["set"; "cpufreq"; "governor"]
   "org.freedesktop.DBus" -> ["org"; "freedesktop"; "dbus"] *)
let split name =
  let len = String.length name in
  let rec find_end_word previous_is_upper i =
    if i = len
    then (i, i)
    else match name.[i] with
      | '.' -> (i, i + 1)
      | ch when ch >= 'A' && ch <= 'Z' -> begin
          match previous_is_upper with
            | true -> find_end_word true (i + 1)
            | false -> (i, i)
        end
      | _ -> find_end_word false (i + 1)
  in
  let rec split i =
    if i = len
    then []
    else
      let j, k = find_end_word true (i + 1) in
      String.lowercase (String.sub name i (j - i)) :: split k
  in
  split 0

let rec print_parts pp = function
  | [] -> ()
  | [e] -> pp_print_string pp e
  | e :: l -> fprintf pp "%s_%a" e print_parts l

let plid pp str = print_parts pp (split str)
let puid pp str = match split str with
  | [] -> ()
  | e :: l -> print_parts pp (String.capitalize e :: l)

(***** Printing of module interfaces *****)

(* We do not use camlp4 because we must print implementation using the
   syntax extension *)

open Term

let term_of_args = List.map (fun (name, typ) -> interf_term_of_single typ)

let print_interf pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a : sig\n" puid name;
  p "  type t = OBus_proxy.t\n";
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  val %a : %a\n" plid name
          (print_func (term "Lwt.t" [tuple (term_of_args  outs)]))
          (term "t" [] :: term_of_args ins)
    | _ -> ()
  end content;
  p "end\n"

(***** Printing of module implementations *****)

let term_of_args = List.map (fun (name, typ) -> implem_term_of_single typ)

let print_implem sugar pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a = struct\n" puid name;
  p "  include OBus_client.Make(struct let name = %S end)\n" name;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        if sugar then
          p "  let %a = call %S << %a >>\n" plid name name
            (print_func (tuple (term_of_args  outs)))
            (term_of_args ins)
        else
          p "  let %a = call %S %a\n" plid name name
            (print_func_no_sugar (tuple (term_of_args  outs)))
            (term_of_args ins)
    | _ -> ()
  end content;
  p "end\n"
