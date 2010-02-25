(*
 * oBus_introspect.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_xml_parser

type name = string

type annotation = name * string
type argument = name option * OBus_value.tsingle

type access = Read | Write | Read_write

type declaration =
  | Method of name * argument list * argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * OBus_value.tsingle * access * annotation list

type interface = name * declaration list * annotation list
type node = OBus_path.element
type document = interface list * node list

exception Parse_failure = OBus_xml_parser.Parse_failure

let () =
  Printexc.register_printer
    (function
       | Parse_failure((line, column), msg) ->
           Some(Printf.sprintf "failed to parse D-Bus introspection document, at line %d, column %d: %s" line column msg)
       | _ ->
           None)

let annotations =
  any (elt "annotation"
         (perform
            name <-- ar "name";
            value <-- ar "value";
            return (name, value)))

type direction = In | Out

let atype =
  ar "type" >>=
    (fun signature -> match OBus_value.signature_of_string signature with
       | [] -> failwith "empty signature"
       | [t] -> return t
       | _ -> Printf.ksprintf failwith "this signature contains more than one single type: %S" signature)

let arguments =
  any (elt "arg"
         (perform
            name <-- ao "name";
            dir <-- afd "direction" In [("in", In); ("out", Out)];
            typ <-- atype;
            return (dir, (name, typ))))

let mk_aname test =
  ar "name" >>= fun name ->
    match test name with
      | Some error -> failwith (OBus_string.error_message error)
      | None -> return name

let amember = mk_aname OBus_name.validate_member
let anode = mk_aname OBus_path.validate_element
let ainterface = mk_aname OBus_name.validate_interface

let method_decl =
  elt "method"
    (perform
       name <-- amember;
       (ins, outs) <-- arguments >>= (fun args ->
                                        return (OBus_util.split (function
                                                                   | (In, x) -> OBus_util.Left x
                                                                   | (Out, x) -> OBus_util.Right x) args));
       annots <-- annotations;
       return (Method(name, ins, outs, annots)))

let signal_decl =
  elt "signal"
    (perform
       name <-- amember;
       args <-- arguments;
       annots <-- annotations;
       return (Signal(name, List.map snd args, annots)))

let property_decl =
  elt "property"
    (perform
       name <-- amember;
       access <-- afr "access" [("read", Read); ("write", Write); ("readwrite", Read_write)];
       typ <-- atype;
       annots <-- annotations;
       return (Property(name, typ, access, annots)))

let node =
  elt "node" (perform
                name <-- anode;
                match OBus_path.validate_element name with
                  | None -> return name
                  | Some error -> failwith (OBus_string.error_message { error with OBus_string.typ = "node name" }))

let interface =
  elt "interface"
    (perform
       name <-- ainterface;
       decls <-- any (union [method_decl;
                             signal_decl;
                             property_decl]);
       annots <-- annotations;
       return (name, decls, annots))

let document =
  elt "node"
    (perform
       interfs <-- any interface;
       subs <-- any node;
       return (interfs, subs))

let input xi = OBus_xml_parser.input xi document

type xml = Element of string * (string * string) list * xml list

let to_xml (ifaces, nodes) =
  let pannots = List.map (fun (n, v) -> Element("annotation", [("name", n); ("value", v)], [])) in
  let pargs dir = List.map (fun (n, t) ->
                             let attrs = [("type", OBus_value.string_of_signature [t])] in
                             let attrs = match dir with
                               | Some In -> ("direction", "in") :: attrs
                               | Some Out -> ("direction", "out") :: attrs
                               | None -> attrs in
                             let attrs = match n with
                               | Some n -> ("name", n) :: attrs
                               | None -> attrs in
                             Element("arg", attrs, [])) in
  Element("node", [],
          List.map (fun (name, content, annots) ->
                      Element("interface", [("name", name)],
                              List.map
                                (function
                                   | Method(name, ins, outs, annots) ->
                                       Element("method", [("name", name)],
                                               pargs (Some In) ins
                                               @ pargs (Some Out) outs
                                               @ pannots annots)
                                   | Signal(name, args, annots) ->
                                       Element("signal", [("name", name)],
                                               pargs None args
                                               @ pannots annots)
                                   | Property(name, typ, access, annots) ->
                                       Element("property",
                                               [("name", name);
                                                ("type", OBus_value.string_of_signature [typ]);
                                                ("access", match access with
                                                   | Read -> "read"
                                                   | Write -> "write"
                                                   | Read_write -> "readwrite")],
                                               pannots annots))
                                content
                              @ pannots annots)) ifaces
          @ List.map (fun n -> Element("node", [("name", n)], [])) nodes)

let output xo doc =
  let rec aux (Element(name, attrs, children)) =
    Xmlm.output xo (`El_start(("", name), List.map (fun (name, value) -> (("", name), value)) attrs));
    List.iter aux children;
    Xmlm.output xo `El_end
  in
  Xmlm.output xo (`Dtd(Some "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"\n\
                             \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">"));
  aux (to_xml doc)

let obus_document = OBus_type.map OBus_pervasives.obus_string
  (fun str ->
     try
       input (Xmlm.make_input ~strip:true (`String(0, str)))
     with Xmlm.Error((line, column), err) ->
       raise (OBus_type.Cast_failure("OBus_introspect.obus_document",
                                     Printf.sprintf
                                       "invalid document, at line %d: %s"
                                       line (Xmlm.error_message err))))
  (fun doc ->
     let buf = Buffer.create 42 in
     output (Xmlm.make_output ~nl:true ~indent:(Some 2) (`Buffer buf)) doc;
     Buffer.contents buf)
