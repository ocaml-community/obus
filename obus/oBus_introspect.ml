(*
 * oBus_introspect.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Xml
open OBus_type
open Xparser

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

type parsing_error = Xparser.error
exception Parse_failure = Xparser.Parse_failure

let print_error = Xparser.print_error

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
       | _ -> failwith (Printf.sprintf "this signature contains more than one single type: %S" signature))

let arguments =
  any (elt "arg"
         (perform
            name <-- ao "name";
            dir <-- afd "direction" In [("in", In); ("out", Out)];
            typ <-- atype;
            return (dir, (name, typ))))

let method_decl =
  elt "method"
    (perform
       name <-- ar "name";
       (ins, outs) <-- arguments >>= (fun args ->
                                        return (Util.split (function
                                                              | (In, x) -> Util.Left x
                                                              | (Out, x) -> Util.Right x) args));
       annots <-- annotations;
       return (Method(name, ins, outs, annots)))

let signal_decl =
  elt "signal"
    (perform
       name <-- ar "name";
       args <-- arguments;
       annots <-- annotations;
       return (Signal(name, List.map snd args, annots)))

let property_decl =
  elt "property"
    (perform
       name <-- ar "name";
       access <-- afr "access" [("read", Read); ("write", Write); ("readwrite", Read_write)];
       typ <-- atype;
       annots <-- annotations;
       return (Property(name, typ, access, annots)))

let node = elt "node" (ar "name")

let interface =
  elt "interface"
    (perform
       name <-- ar "name";
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

let of_xml = parse document

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

let tdocument = wrap_basic tstring
  (fun x ->
     let p = XmlParser.make () in
     XmlParser.prove p false;
     of_xml (XmlParser.parse p (XmlParser.SString x)))
  (fun x -> Xml.to_string_fmt (to_xml x))
