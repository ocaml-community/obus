(*
 * lmap.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
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

let regexp = Str.regexp "\\."

let extract name def_mappings =
  match List.partition (fun ((_, n), _, _) -> n = name) def_mappings with
    | [v], l -> (Some(v), l)
    | [], l -> (None, l)
    | _ -> assert false

let add mappings (dbus_name, lang_name, x) =
  let rec aux (Node(mappings)) = function
    | [] -> raise (Invalid_argument "invalid module name")
    | [name] -> Node(match extract name mappings with
                       | Some((_, name), maps, sons), l -> ((dbus_name, name), x, sons) :: l
                       | None, l -> ((dbus_name, name), x, Node []) :: l)
    | name :: names -> Node(match extract name mappings with
                              | Some((dn, name), maps, sons), l -> ((dn, name), maps, aux sons names) :: l
                              | None, l -> (("", name), [], aux (Node []) names) :: l)
  in
    aux mappings (Str.split regexp lang_name)

let merge (Mapping(la, a)) (Mapping(lb, b)) =
  let rec aux (Node(a)) (Node(b)) =
    Node begin List.fold_left begin fun acc (((dnb, nb), mapsb, sonsb) as b) ->
      match extract nb acc with
        | Some((dna, na), mapsa, sonsa), l -> begin match mapsb with
            | [] -> ((dna, na), mapsa, aux sonsa sonsb)
            | _ -> ((dnb, nb), mapsb, aux sonsa sonsb)
          end :: l
        | None, l -> b :: l
    end a b end
  in
    if la = lb
    then raise (Invalid_argument "cannot merge mapping from different languages")
    else Mapping(la, aux a b)

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

  let rec main_to_xml lang_names (Node(mappings)) =
    List.flatten begin List.map begin fun ((dbus_name, lang_name), content, sons) ->
      let lang_names = lang_name :: lang_names in
        Element("interface", name_mapping_to_params (dbus_name, Util.rjoin "." lang_names),
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
                end content) :: main_to_xml lang_names sons
    end mappings end
  in

  let remove_empty interfaces =
    List.filter (function
                   | Element("interface", _, []) -> false
                   | _ -> true) interfaces
  in

    (fun (Mapping(language, mapping)) ->
       Element("map", [("language", language)],
               remove_empty (main_to_xml [] mapping)))

let from_xml type_reader xml =
  let args_parser () =
    s1 (union
          [elt "arg" [< "dname"; "name"; "dtype"; "type" >]
             s0
             (fun dname name dtyp typ ->
                Arg((dname, name), DBus.type_of_string dtyp, type_reader typ));
           elt "pack" [< "type" >]
             (s1 (any (elt "arg" [< "dname"; "name"; "dtype"; "type" >]
                         s0
                         (fun dname name dtyp typ ->
                            ((dname, name), DBus.type_of_string dtyp, type_reader typ)))))
             (fun typ l -> Pack(l, type_reader typ))])
  in
    parse (elt "map" [< "language" >]
             (s1 (any (elt "interface" [< "dname"; "name" >]
                         (s1 (union
                                [elt "method" [< "dname"; "name" >]
                                   (s2
                                      (one (elt "params" [<>]
                                              (args_parser ())
                                              (fun x -> x)))
                                      (one (elt "result" [<>]
                                              (args_parser ())
                                              (fun x -> x))))
                                   (fun dname name ins outs -> Method((dname, name), ins, outs));
                                 elt "signal" [< "dname"; "name" >]
                                   (args_parser ())
                                   (fun dname name args -> Signal((dname, name), args))]))
                         (fun dname name defs -> (dname, name, defs)))))
             (fun language mappings -> Mapping(language,
                                               List.fold_left add (Node []) mappings))) xml
