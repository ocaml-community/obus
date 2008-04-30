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

type 'a t = {
  language : string;
  sigs : 'a Sig.tree;
  map : (string * DBus.interface) list;
}

open Xml
open Xparser

let empty language =
  { language = language;
    sigs = Sig.Tree [];
    map = [] }

let merge a b =
  if a.language <> b.language
  then raise (Invalid_argument "cannot merge mapping from different languages")
  else { language = a.language;
         sigs = Sig.merge a.sigs b.sigs;
         map = b.map @ a.map }

let to_xml type_writer mapping =
  let names dname lname = [("dname", dname); ("name", lname)] in

  let rec args_to_xml dir dargs largs =
    List.map2 begin fun (Sig.Arg(dname, dtype)) (Sig.Arg(lname, ltype)) ->
      Element("arg", names dname lname
                @ (match dir with Some(d) -> [("direction", d)] | None -> [])
                @ [("dtype", DBus.signature_of_dtypes dtype);
                   ("type", type_writer ltype)], [])
    end dargs largs
  in

  let rec main_to_xml lnames (Sig.Tree(nodes)) =
    List.flatten begin List.map begin fun (Sig.Sig(lshortname, ldefs), t) ->
      let lnames = lshortname :: lnames in
      let lname = Util.rjoin "." lnames in
      let rest = main_to_xml lnames t in
        match ldefs with
          | [] -> rest
          | _ -> let (Sig.Sig(dname, ddefs)) = List.assoc lname mapping.map in
              Element("interface", names dname lname,
                      List.map2 begin fun a b -> match a, b with
                        | Sig.Method(dn, dins, douts), Sig.Method(ln, lins, louts) ->
                            Element("method", names dn ln,
                                    args_to_xml (Some "in") dins lins
                                    @ args_to_xml (Some "out") douts louts)
                        | Sig.Signal(dn, dargs), Sig.Signal(ln, largs) ->
                            Element("signal", names dn ln,
                                    args_to_xml None dargs largs)
                        | _ -> assert false
                      end ddefs ldefs) :: rest
    end nodes end
  in

  let remove_empty interfaces =
    List.filter (function
                   | Element("interface", _, []) -> false
                   | _ -> true) interfaces
  in
    Element("map", [("language", mapping.language)],
            remove_empty (main_to_xml [] mapping.sigs))

let from_xml type_reader xml =
  parse (elt "map" [< (P"language") >]
           (s1 (any (elt "interface" [< (P"dname"); (P"name") >]
                       (s1 (union
                              [elt "method" [< (P"dname"); (P"name") >]
                                 (s2
                                    (any (elt "arg" [< (P"dname"); (P"name"); (A("direction", "in", ["in"])); (P"dtype"); (P"type") >]
                                            s0
                                            (fun dname lname _ dtype ltype ->
                                               (Sig.Arg(dname, DBus.dtypes_of_signature dtype),
                                                Sig.Arg(lname, type_reader ltype)))))
                                    (any (elt "arg" [< (P"dname"); (P"name"); (A("direction", "in", ["out"])); (P"dtype"); (P"type") >]
                                            s0
                                            (fun dname lname _ dtype ltype ->
                                               (Sig.Arg(dname, DBus.dtypes_of_signature dtype),
                                                Sig.Arg(lname, type_reader ltype))))))
                                 (fun dname lname ins outs ->
                                    let dins, lins = List.split ins
                                    and douts, louts = List.split outs in
                                      (Sig.Method(dname, dins, douts),
                                       Sig.Method(lname, lins, louts)));
                               elt "signal" [< (P"dname"); (P"name") >]
                                 (s1 (any (elt "arg" [< (P"dname"); (P"name"); (P"dtype"); (P"type") >]
                                             s0
                                             (fun dname lname dtype ltype ->
                                                (Sig.Arg(dname, DBus.dtypes_of_signature dtype),
                                                 Sig.Arg(lname, type_reader ltype))))))
                                 (fun dname lname args ->
                                    let dargs, largs = List.split args in
                                      (Sig.Signal(dname, dargs),
                                       Sig.Signal(lname, largs)))]))
                       (fun dname lname defs ->
                          let ddefs, ldefs = List.split defs in
                            ((lname, Sig.Sig(dname, ddefs)),
                             (lname, ldefs))))))
           (fun language sigs ->
              let map, sigs = List.split sigs in
                { language = language;
                  sigs = List.fold_left (fun acc (name, defs) -> Sig.add name defs acc) (Sig.Tree []) sigs;
                  map = map } )) xml
