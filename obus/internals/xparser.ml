(*
 * xparser.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Xml
open Format

type attributes = (string * string) list
type stack = (string * attributes) list
type error = stack * string
exception Parse_failure of error

let print_error pp (stack, msg) =
  match stack with
    | [] ->
        fprintf pp "at the root of the document:\n-> %s\n" msg
    | _ ->
        fprintf pp "in the following element:\n";
        ignore (List.fold_left
                  (fun indent (name, attrs) ->
                     fprintf pp "%s<%s" indent name;
                     List.iter (fun (k, v) -> fprintf pp " %s=\"%s\"" k v) attrs;
                     fprintf pp ">\n";
                     indent ^ "  ") "" stack);
        fprintf pp "-> %s\n" msg

type 'a result =
  | Value of 'a
  | Error of stack * string

type node_type =
  | NT_element of string
  | NT_pcdata
  | NT_any
  | NT_union of node_type list

type 'a node = node_type * (Xml.xml -> 'a result option)

type 'a t = Xml.xml list -> attributes -> Xml.xml list * 'a result
let bind m f xmls attrs =
  let rest, result = m xmls attrs in
  match result with
    | Value v -> f v rest attrs
    | Error _ as e -> (rest, e)
let return v xmls attrs = (xmls, Value v)
let failwith msg xmls attrs = (xmls, Error([], msg))

let (>>=) = bind

let ao name xmls attrs =
  (xmls, Value (Util.assoc name attrs))

let ar name =
  ao name >>= (function
                 | Some v -> return v
                 | None -> Printf.ksprintf failwith "attribute '%s' missing" name)

let ad name default =
  ao name >>= (function
                 | Some v -> return v
                 | None -> return default)

let af name field value = match Util.assoc value field with
  | Some v -> return v
  | None -> Printf.ksprintf failwith "unexpected value for '%s' (%s), must be one of %s"
      name value (String.concat ", " (List.map fst field))

let afr name field = ar name >>= af name field
let afo name field = ao name >>= function
  | Some v -> af name field v >>= (fun x -> return (Some x))
  | None -> return None

let afd name default field =
  ao name >>= (function
                 | Some v -> af name field v
                 | None -> return default)

let execute xml_parser xmls attrs =
  try
    match xml_parser xmls attrs with
      | [], result -> result
      | _, (Error _ as err) -> err
      | x :: _, _ -> Error([], "unknown element: " ^
                             match x with
                               | Element(n, _, _) -> n
                               | PCData _ -> "<pcdata>")
  with
      exn -> Error([], Printexc.to_string exn)

let elt name elt_parser =
  (NT_element name,
   function
     | Element(name', attrs, children) when name = name' ->
         Some(match execute elt_parser children attrs with
                | Error(stack, error) ->
                    Error((name, attrs) :: stack, error)
                | x -> x)
    | _ -> None)

let pcdata =
  (NT_pcdata,
   function
     | Element _ -> None
     | PCData x -> Some(Value x))

let raw =
  (NT_any, (fun x -> Some(Value x)))

let union nodes =
  let types, fl = List.split nodes in
  (NT_union types, fun node -> Util.find_map (fun f -> f node) fl)

let wrap (typ, f) g = (typ, fun node ->
                         Util.wrap_option (f node)
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

let opt (typ, f) xmls attrs =
  match Util.part_map f xmls with
    | [], rest -> (rest, Value None)
    | [Value x], rest -> (rest, Value (Some x))
    | [Error _ as err], rest -> (rest, err)
    | _, rest -> (rest, Error([], "too many nodes of type: " ^ string_of_type typ))

let one (typ, f) =
  opt (typ, f) >>= function
    | Some x -> return x
    | None -> Printf.ksprintf failwith "element missing: %s" (string_of_type typ)

let rec any (typ, f) xmls attrs =
  let success, rest = Util.part_map f xmls in
  (rest,
   List.fold_right
     (fun result acc -> match result, acc with
        | Value v, Value l -> Value (v :: l)
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err) success (Value []))

let parse node xml =
  match execute (one node) [xml] [] with
    | Value x -> x
    | Error(stack, msg) -> raise (Parse_failure(stack, msg))
