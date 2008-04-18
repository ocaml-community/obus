(*
 * lmap.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type name = string
type name_mapping = name * name

type 'a param_mapping =
  | Arg of name_mapping * DBus.typ * 'a
  | Pack of (name_mapping * DBus.typ * 'a) list * 'a

type 'a def_mapping =
  | Method of name_mapping * 'a param_mapping list * 'a param_mapping list
  | Signal of name_mapping * 'a param_mapping list

type 'a tree =
  | Node of (name_mapping * 'a def_mapping list * 'a tree) list

type 'a t =
  | Mapping of string * 'a tree

open Xml
open Xparser

let to_xml type_writer =
  let name_mapping_to_params (dname, name) = [("dname", dname); ("name", name)] in

  let rec args_to_xml args =
    List.map begin function
      | Arg(nm, dt, t) ->
          Element("arg", name_mapping_to_params nm
                    @ [("dtype", DBus.string_of_type dt);
                       ("type", type_writer t)], [])
      | Pack(nmdts, t) ->
          Element("pack", [("type", type_writer t)],
                  args_to_xml (List.map (fun (x, y, z) -> Arg(x, y, z)) nmdts))
    end args
  in

  let rec main_to_xml (Node(mappings)) =
    List.flatten begin List.map begin fun (nm, content, sons) ->
      Element("interface", name_mapping_to_params nm,
              List.map begin function
                | Method(nm, ins, outs) ->
                    Element("method", name_mapping_to_params nm,
                            [Element("params", [],
                                     args_to_xml ins);
                             Element("result", [],
                                     args_to_xml outs)])
                | Signal(nm, args) ->
                    Element("signal", name_mapping_to_params nm,
                            args_to_xml args)
              end content) :: main_to_xml sons
    end mappings end
  in

  let remove_empty interfaces =
    List.filter (function
                   | Element("interface", _, []) -> false
                   | _ -> true) interfaces
  in

    (fun (Mapping(language, mapping)) ->
       Element("map", [("language", language)],
               remove_empty (main_to_xml mapping)))

let from_xml type_reader xml =
  let args_parser () =
    s1 (union
          [elt "arg" (p4 "dname" "name" "dtype" "type")
             s0
             (fun dname name dtyp typ ->
                Arg((dname, name), DBus.type_of_string dtyp, type_reader typ));
           elt "pack" (p1 "type")
             (s1 (any (elt "arg" (p4 "dname" "name" "dtype" "type")
                         s0
                         (fun dname name dtyp typ ->
                            ((dname, name), DBus.type_of_string dtyp, type_reader typ)))))
             (fun typ l -> Pack(l, type_reader typ))])
  in
    parse (elt "map" (p1 "language")
             (s1 (any (elt "interface" (p2 "dname" "name")
                         (s1 (union
                                [elt "method" (p2 "dname" "name")
                                   (s2
                                      (one (elt "params" p0
                                              (args_parser ())
                                              (fun x -> x)))
                                      (one (elt "result" p0
                                              (args_parser ())
                                              (fun x -> x))))
                                   (fun dname name ins outs -> Method((dname, name), ins, outs));
                                 elt "signal" (p2 "dname" "name")
                                   (args_parser ())
                                   (fun dname name args -> Signal((dname, name), args))]))
                         (fun dname name defs -> ((dname, name), defs, Node [])))))
             (fun language mappings -> Mapping(language, Node mappings))) xml
