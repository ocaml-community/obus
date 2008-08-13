(*
 * oBus_introspect.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
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
type node = name
type document = interface list * node list

let ($) a b = a b

module Make_parser(Parser : OBus_xml_parser.S) =
struct
  open Parser

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
                                          return $ Util.split (function
                                                                 | (In, x) -> Util.Left x
                                                                 | (Out, x) -> Util.Right x) args);
         annots <-- annotations;
         return $ Method(name, ins, outs, annots))

  let signal_decl =
    elt "signal"
      (perform
         name <-- ar "name";
         args <-- arguments;
         annots <-- annotations;
         return $ Signal(name, List.map snd args, annots))

  let property_decl =
    elt "property"
      (perform
         name <-- ar "name";
         access <-- afr "access" [("read", Read); ("write", Write); ("readwrite", Read_write)];
         typ <-- atype;
         annots <-- annotations;
         return $ Property(name, typ, access, annots))

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
end
