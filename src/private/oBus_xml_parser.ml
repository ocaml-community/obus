(*
 * oBus_xml_parser.ml
 * ------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf

type attributes = (string * string) list
exception Parse_failure of Xmlm.pos * string

type xml =
  | Element of Xmlm.pos * string * (string * string) list * xml list
  | PCData of Xmlm.pos * string

type 'a result =
  | Value of 'a
  | Error of Xmlm.pos * string

type node_type =
  | NT_element of string
  | NT_pcdata
  | NT_any
  | NT_union of node_type list

type 'a node = node_type * (xml -> 'a result option)

type 'a t = Xmlm.pos -> xml list -> attributes -> xml list * 'a result
let bind m f pos xmls attrs =
  let rest, result = m pos xmls attrs in
  match result with
    | Value v -> f v pos rest attrs
    | Error _ as e -> (rest, e)
let return v pos xmls attrs = (xmls, Value v)
let failwith msg pos xmls attrs = (xmls, Error(pos, msg))

let (>>=) = bind

let ao name pos xmls attrs =
  (xmls, Value(OBus_util.assoc name attrs))

let ar name =
  ao name >>= function
    | Some v -> return v
    | None -> ksprintf failwith  "attribute '%s' missing" name

let ad name default =
  ao name >>= function
    | Some v -> return v
    | None -> return default

let afo name field pos xmls attrs = match OBus_util.assoc name attrs with
  | None ->
      (xmls, Value None)
  | Some v ->
      match OBus_util.assoc v field with
        | Some v ->
            (xmls, Value(Some v))
        | None ->
            (xmls, Error(pos, sprintf "unexpected value for '%s' (%s), must be one of %s"
                           name v (String.concat ", " (List.map (fun (name, v) -> "'" ^ name ^ "'") field))))

let afr name field =
  afo name field >>= function
    | Some v -> return v
    | None -> ksprintf failwith  "attribute '%s' missing" name

let afd name default field =
  afo name field >>= function
    | Some v -> return v
    | None -> return default

let execute xml_parser pos xmls attrs =
  try
    match xml_parser pos xmls attrs with
      | [], result ->
          result
      | _, (Error _ as error) ->
          error
      | Element(pos, name, _, _) :: _, _ ->
          Error(pos, sprintf "unknown element '%s'" name)
      | PCData(pos, _) :: _, _ ->
          Error(pos, "trailing pc-data")
  with exn ->
    Error(pos, Printexc.to_string exn)

let elt name elt_parser =
  (NT_element name,
   function
     | Element(pos, name', attrs, children) when name = name' ->
         Some(execute elt_parser pos children attrs)
     | _ ->
         None)

let pcdata =
  (NT_pcdata,
   function
     | Element _ -> None
     | PCData(_, x) -> Some(Value x))

let union nodes =
  let types, fl = List.split nodes in
  (NT_union types, fun node -> OBus_util.find_map (fun f -> f node) fl)

let map (typ, f) g = (typ, fun node ->
                        OBus_util.map_option (f node)
                          (function
                             | Value v -> Value (g v)
                             | Error _ as e -> e))

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

let opt (typ, f) pos xmls attrs =
  match OBus_util.part_map f xmls with
    | [], rest -> (rest, Value None)
    | [Value x], rest -> (rest, Value (Some x))
    | [Error _ as err], rest -> (rest, err)
    | _, rest -> (rest, Error(pos, "too many nodes of type: " ^ string_of_type typ))

let one (typ, f) =
  opt (typ, f) >>= function
    | Some x -> return x
    | None -> ksprintf failwith "element missing: %s" (string_of_type typ)

let rec any (typ, f) pos xmls attrs =
  let success, rest = OBus_util.part_map f xmls in
  (rest,
   List.fold_right
     (fun result acc -> match result, acc with
        | Value v, Value l -> Value (v :: l)
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err) success (Value []))

let pos_of_xml = function
  | Element(pos, _, _, _) -> pos
  | PCData(pos, _) -> pos

let parse node xml =
  match execute (one node) (pos_of_xml xml) [xml] [] with
    | Value x -> x
    | Error(pos, msg) -> raise (Parse_failure(pos, msg))

let input input node =
  let rec make () =
    let pos = Xmlm.pos input in
    match Xmlm.input input with
      | `El_start((uri, name), attrs) ->
          Element(pos, name, List.map (fun ((uri, name), value) -> (name, value)) attrs, make_list ())
      | `El_end ->
          raise (Parse_failure(pos, "unexpected end of element"))
      | `Data str ->
          PCData(pos, str)
      | `Dtd _ ->
          make ()
  and make_list () =
    let pos = Xmlm.pos input in
    match Xmlm.input input with
      | `El_start((uri, name), attrs) ->
          let xml = Element(pos, name, List.map (fun ((uri, name), value) -> (name, value)) attrs, make_list ()) in
          xml :: make_list ()
      | `El_end ->
          []
      | `Data str ->
          let xml = PCData(pos, str) in
          xml :: make_list ()
      | `Dtd _ ->
          make_list ()
  in
  try
    parse node (make ())
  with Xmlm.Error(pos, error) ->
    raise (Parse_failure(pos, Xmlm.error_message error))
