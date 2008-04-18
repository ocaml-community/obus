(*
 * xml_parser.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

exception Parse_failed

open Xml

type 'a xml_parser = xml -> 'a option
type ('a, 'b) param_parser = (string * string) list -> 'a -> 'b
type 'a seq_elt_parser = xml list -> 'a * xml list
type ('a, 'b) seq_parser = xml list -> 'a -> 'b * xml list

let pn l f = f

let pc param_name params_parser l f =
  (params_parser l) (f (List.assoc param_name l))

let elt elt_name params_parser sons_parser f = function
  | Element(name, args, sons) when name = elt_name ->
      begin try
        match sons_parser sons (params_parser args f) with
          | v, [] -> Some(v)
          | _ -> None
      with
          Parse_failed -> None
      end
  | _ -> None

let one xml_parser xmls =
  match Util.part_map xml_parser xmls with
    | [v], l -> (v, l)
    | _ -> raise Parse_failed

let any xml_parser xmls =
  Util.part_map xml_parser xmls

let opt xml_parser xmls =
  match Util.part_map xml_parser xmls with
    | [], l -> (None, l)
    | [v], l -> (Some(v), l)
    | _ -> raise Parse_failed

let sn xmls f = (f, xmls)

let sc seq_elt_parser seq_parser xmls f =
  let g, rest = seq_elt_parser xmls in
    seq_parser rest (f g)

let parse xml_parser xml =
  match xml_parser xml with
    | Some(v) -> v
    | None -> raise Parse_failed
