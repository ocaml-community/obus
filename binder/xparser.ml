(*
 * xparser.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Xml

type parser_type =
  | PT_elt of string
  | PT_pcdata

type attributes = (string * string) list

type error =
  | Attribute_missing of string
  | Attribute_invalid of string * string * string list
  | Element_unknown of xml
  | Child_invalid_number of string * parser_type

exception Error of (string * attributes) list * error

let fail err = raise (Error([], err))

type 'a xml_parser = parser_type * (xml -> 'a option)
type 'a attribute_parser = attributes -> 'a
type ('a, 'b) attributes_parser = attributes -> 'a -> 'b
type 'a seq_elt_parser = xml list -> 'a * xml list
type ('a, 'b) seq_parser = xml list -> 'a -> 'b * xml list

let a name f attributes =
  f (Util.assoc name attributes)

let ar name f attributes =
  match Util.assoc name attributes with
    | Some v -> f v
    | None -> fail (Attribute_missing name)

let ao name f attributes =
  match Util.assoc name attributes with
    | Some v -> Some (f v)
    | None -> None

let ad name default f attributes =
  match Util.assoc name attributes with
    | Some v -> f v
    | None -> default

let af name field value = match Util.assoc value field with
  | Some v -> v
  | None -> fail (Attribute_invalid(name, value, List.map fst field))

let afr name field = ar name (af name field)
let afo name field = ao name (af name field)
let afd name default field = ad name default (af name field)

let id x = x
let fid l = List.combine l l

let ars name = ar name id
let aos name = ao name id
let ads name default = ad name default id
let afrs name field = afr name (fid field)
let afos name field = afo name (fid field)
let afds name default field = afd name default (fid field)

let an attrs f = f
let ac ah at attrs f = at attrs (f (ah attrs))

let elt elt_name attrs_parser sons_parser f =
  (PT_elt elt_name,
   fun elt -> match elt with
     | Element(name, attrs, sons) when name = elt_name ->
         begin try
           match sons_parser sons (attrs_parser attrs f) with
             | v, [] -> Some(v)
             | _, elt :: l -> fail (Element_unknown elt)
         with
           | Error(stack, e) -> raise (Error((name, attrs) :: stack, e))
         end
     | _ -> None)

let pcdata f =
  (PT_pcdata,
   function
     | PCData s -> Some (f s)
     | _ -> None)

let text = pcdata id

let one xml_parser xmls =
  match Util.part_map (snd xml_parser) xmls with
    | [v], l -> (v, l)
    | [], _ -> fail (Child_invalid_number("missing child", (fst xml_parser)))
    | _ -> fail (Child_invalid_number("too many children", (fst xml_parser)))

let any xml_parser xmls =
  Util.part_map (snd xml_parser) xmls

let opt xml_parser xmls =
  match Util.part_map (snd xml_parser) xmls with
    | [], l -> (None, l)
    | [v], l -> (Some(v), l)
    | _ -> fail (Child_invalid_number("too many children", (fst xml_parser)))

let union xml_parsers xmls =
  Util.part_map (fun xml ->
                   match Util.filter_map (fun xml_parser -> (snd xml_parser) xml) xml_parsers with
                     | [v] -> Some(v)
                     | _ -> None) xmls

let sn xmls f = (f, xmls)

let sc seq_elt_parser seq_parser xmls f =
  let g, rest = seq_elt_parser xmls in
    seq_parser rest (f g)

open Log

let print_error filename stack err =
  print "error encountered while parsing %s:\n"
    (match filename with
       | Some s -> "file \"" ^ s ^ "\""
       | None -> "xml document");
  begin match stack with
    | [] -> ()
    | _ ->
        print "in the folowing element:\n";
        ignore (List.fold_left
                  (fun indent (name, attrs) ->
                     print "%s<%s" indent name;
                     List.iter (fun (k, v) -> print " %s=\"%s\"" k v) attrs;
                     print ">\n";
                     indent ^ "  ") "" stack)
  end;
  match err with
    | Attribute_missing(name) ->
        print "argument '%s' missing\n" name
    | Attribute_invalid(key, value, field) ->
        print "unexpected value for '%s' (%s), must be one of %s\n"
          key value
          (String.concat ", " field)
    | Element_unknown(elt) ->
        print "unexpected element:\n%s\n" (Xml.to_string_fmt elt)
    | Child_invalid_number(msg, typ) ->
        print "%s of type: %s" msg
          (match typ with
             | PT_elt name -> name
             | PT_pcdata -> "PCData")


let parse xml_parser ?filename xml =
  try
    match (snd xml_parser) xml with
      | Some(v) -> v
      | None -> fail (Element_unknown xml)
  with
      Error(stack, err) ->
        print_error filename stack err;
        exit 2

let s0 = sn
let s1 x1 = sc x1 (sn)
let s2 x2 x1 = sc x2 (sc x1 (sn))
let s3 x3 x2 x1 = sc x3 (sc x2 (sc x1 (sn)))
let s4 x4 x3 x2 x1 = sc x4 (sc x3 (sc x2 (sc x1 (sn))))
let s5 x5 x4 x3 x2 x1 = sc x5 (sc x4 (sc x3 (sc x2 (sc x1 (sn)))))
let a0 = an
let a1 x1 = ac x1 (an)
let a2 x2 x1 = ac x2 (ac x1 (an))
let a3 x3 x2 x1 = ac x3 (ac x2 (ac x1 (an)))
let a4 x4 x3 x2 x1 = ac x4 (ac x3 (ac x2 (ac x1 (an))))
let a5 x5 x4 x3 x2 x1 = ac x5 (ac x4 (ac x3 (ac x2 (ac x1 (an)))))
let a6 x6 x5 x4 x3 x2 x1 = ac x6 (ac x5 (ac x4 (ac x3 (ac x2 (ac x1 (an))))))
let a7 x7 x6 x5 x4 x3 x2 x1 = ac x7 (ac x6 (ac x5 (ac x4 (ac x3 (ac x2 (ac x1 (an)))))))
let a8 x8 x7 x6 x5 x4 x3 x2 x1 = ac x8 (ac x7 (ac x6 (ac x5 (ac x4 (ac x3 (ac x2 (ac x1 (an))))))))
