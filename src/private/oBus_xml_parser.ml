(*
 * oBus_xml_parser.ml
 * ------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf

exception Parse_failure of Xmlm.pos * string

type xml =
  | Element of Xmlm.pos * string * (string * string) list * xml list
  | PCData of Xmlm.pos * string

type node_type =
  | NT_element of string
  | NT_pcdata
  | NT_any
  | NT_union of node_type list

type 'a node = node_type * (xml -> 'a option)

type xml_parser = {
  position : Xmlm.pos;
  attributes : (string * string) list;
  mutable children : xml list;
}

let failwith p msg = raise (Parse_failure(p.position, msg))

let ao p name =
  OBus_util.assoc name p.attributes

let ar p name =
  match ao p name with
    | Some v -> v
    | None -> ksprintf (failwith p)  "attribute '%s' missing" name

let ad p name default =
  match ao p name with
    | Some v -> v
    | None -> default

let afo p name field =
  match OBus_util.assoc name p.attributes with
    | None ->
        None
    | Some v ->
        match OBus_util.assoc v field with
          | Some v ->
              Some v
          | None ->
              ksprintf (failwith p)
                "unexpected value for '%s' (%s), must be one of %s"
                name v (String.concat ", " (List.map (fun (name, v) -> "'" ^ name ^ "'") field))

let afr p name field =
  match afo p name field with
    | Some v -> v
    | None -> ksprintf (failwith p)  "attribute '%s' missing" name

let afd p name default field =
  match afo p name field with
    | Some v -> v
    | None -> default

let execute xml_parser p =
  try
    let result = xml_parser p in
    match p.children with
      | [] ->
          result
      | Element(pos, name, _, _) :: _ ->
          ksprintf (failwith p) "unknown element '%s'" name
      | PCData(pos, _) :: _ ->
          failwith p "trailing pc-data"
  with
    | Parse_failure _ as exn ->
        raise exn
    | exn ->
        failwith p (Printexc.to_string exn)

let elt name elt_parser =
  (NT_element name,
   function
     | Element(pos, name', attrs, children) when name = name' ->
         Some(execute elt_parser { position = pos; children = children; attributes =  attrs})
     | _ ->
         None)

let pcdata =
  (NT_pcdata,
   function
     | Element _ -> None
     | PCData(_, x) -> Some x)

let union nodes =
  let types, fl = List.split nodes in
  (NT_union types, fun node -> OBus_util.find_map (fun f -> f node) fl)

let map (typ, f) g = (typ, fun node -> OBus_util.map_option (f node) g)

let string_of_type typ =
  let rec flat acc = function
    | NT_union l -> List.fold_left flat acc l
    | NT_pcdata -> "<pcdata>" :: acc
    | NT_any -> "<any>" :: acc
    | NT_element name -> name :: acc
  in
  match flat [] typ with
    | [] -> "<nothing>"
    | [x] -> x
    | l -> String.concat " or " l

let opt p (typ, f) =
  match OBus_util.part_map f p.children with
    | [], rest ->
        None
    | [x], rest ->
        p.children <- rest;
        Some x
    | _, rest ->
        ksprintf (failwith p) "too many nodes of type %S" (string_of_type typ)

let one p (typ, f) =
  match opt p (typ, f) with
    | Some x -> x
    | None -> ksprintf (failwith p) "element missing: %S" (string_of_type typ)

let rec any p (typ, f) =
  let success, rest = OBus_util.part_map f p.children in
  p.children <- rest;
  success

let pos_of_xml = function
  | Element(pos, _, _, _) -> pos
  | PCData(pos, _) -> pos

let parse node xml =
  execute (fun p -> one p node) { position = pos_of_xml xml; attributes = []; children = [xml] }

let input input node =
  let rec make () =
    let pos = Xmlm.pos input in
    match Xmlm.input input with
      | `El_start(("", name), attrs) ->
          Element(pos, name, List.map (fun ((uri, name), value) -> (name, value)) attrs, make_list ())
      | `El_start((_, name), attrs) ->
          (* Drops elements that are not part of the specification *)
          drop 0;
          make ()
      | `El_end ->
          raise (Parse_failure(pos, "unexpected end of element"))
      | `Data str ->
          PCData(pos, str)
      | `Dtd _ ->
          make ()
  and make_list () =
    let pos = Xmlm.pos input in
    match Xmlm.input input with
      | `El_start(("", name), attrs) ->
          let xml = Element(pos, name, List.map (fun ((uri, name), value) -> (name, value)) attrs, make_list ()) in
          xml :: make_list ()
      | `El_start((_, name), attrs) ->
          drop 0;
          make_list ()
      | `El_end ->
          []
      | `Data str ->
          let xml = PCData(pos, str) in
          xml :: make_list ()
      | `Dtd _ ->
          make_list ()
  and drop deep =
    match Xmlm.input input with
      | `El_start _ ->
          drop (deep + 1)
      | `El_end ->
          if deep <> 0 then drop (deep - 1)
      | `Data str ->
          drop deep
      | `Dtd _ ->
          drop deep
  in
  try
    parse node (make ())
  with Xmlm.Error(pos, error) ->
    raise (Parse_failure(pos, Xmlm.error_message error))
