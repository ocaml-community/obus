(*
 * interface.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

open Xml

type 'a param = string * 'a

type 'a element =
  | Method of string * 'a param list * 'a param list
  | Signal of string * 'a param list

type 'a t = 'a element list

let dtd = Dtd.check (Dtd.parse_string "<!-- DTD for D-BUS Introspection data -->
<!-- (C) 2005-02-02 David A. Wheeler; released under the D-BUS licenses,
         GNU GPL version 2 (or greater) and AFL 1.1 (or greater) -->

<!-- see D-BUS specification for documentation -->

<!ELEMENT node (interface*,node*)>
<!ATTLIST node name CDATA #REQUIRED>

<!ELEMENT interface (annotation*,method*,signal*,property*)>
<!ATTLIST interface name CDATA #REQUIRED>

<!ELEMENT method (annotation*,arg*)>
<!ATTLIST method name CDATA #REQUIRED>

<!ELEMENT arg EMPTY>
<!ATTLIST arg name CDATA #IMPLIED>
<!ATTLIST arg type CDATA #REQUIRED>
<!-- Method arguments SHOULD include \"direction\",
     while signal and error arguments SHOULD not (since there's no point).
     The DTD format can't express that subtlety. -->
<!ATTLIST arg direction (in|out) \"in\">

<!ELEMENT signal (arg*,annotation*)>
<!ATTLIST signal name CDATA #REQUIRED>

<!ELEMENT property (annotation)>  <!-- AKA \"attribute\" -->
<!ATTLIST property name CDATA #REQUIRED>
<!ATTLIST property type CDATA #REQUIRED>
<!ATTLIST property access (read|write|readwrite) #REQUIRED>

<!ELEMENT annotation EMPTY>  <!-- Generic metadata -->
<!ATTLIST annotation name CDATA #REQUIRED>
<!ATTLIST annotation value CDATA #REQUIRED>
")

let xml_parser = XmlParser.make ()
let _ = XmlParser.prove xml_parser false

type direction = In | Out

let name params = List.assoc "name" params
let direction params = match (try List.assoc "direction" params with Not_found -> "in") with
  | "in" -> In
  | "out" -> Out
  | _ -> assert false
let dbus_type params = DBus.type_of_string (List.assoc "type" params)

let from_instrospection lexbuf =
  let arg params = (name params, dbus_type params) in
  let xml = XmlParser.parse xml_parser (XmlParser.SLexbuf lexbuf) in
    match xml with
      | Element("node", _, l) ->
          Util.filter_map begin function
            | Element("interface", params, components) ->
                Some(name params,
                     Util.filter_map begin function
                       | Element("method", params, args) ->
                           Some(Method(name params,
                                       Util.filter_map begin function
                                         | Element("arg", params, _) when direction params = In ->
                                             Some(arg params)
                                         | _ -> None
                                       end args,
                                      Util.filter_map begin function
                                         | Element("arg", params, _) when direction params = Out ->
                                             Some(arg params)
                                         | _ -> None
                                      end args))
                       | Element("signal", params, args) ->
                           Some(Signal(name params,
                                       Util.filter_map begin function
                                         | Element("arg", params, _) ->
                                             Some(arg params)
                                         | _ -> None
                                       end args))
                       | _ -> None
                     end components)
            | _ -> None
          end l
      | _ -> assert false

let map f content =
  let auxp l = List.map (fun (name, typ) -> (name, f typ)) l in
  let rec aux = function
    | Method(name, ins, outs) -> Method(name, auxp ins, auxp outs)
    | Signal(name, args) -> Signal(name, auxp args)
  in
    List.map aux content
