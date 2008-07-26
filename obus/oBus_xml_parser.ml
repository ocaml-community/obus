(*
 * xparser.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Format

module type XML = sig
  type t
  val match_node :
    element:(string -> (string * string) list -> t list -> 'a) ->
    pcdata:(string -> 'a) -> t -> 'a
end

type attributes = (string * string) list
type stack = (string * attributes) list

module type S = sig
  type xml
  type 'a t
  type 'a node
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val failwith : string -> 'a t
  val parse : 'a node -> xml -> [ `Right of 'a | `Left of stack * string ]
  val ar : string -> string t
  val ao : string -> string option t
  val ad : string -> string -> string t
  val afr : string -> (string * 'a) list -> 'a t
  val afo : string -> (string * 'a) list -> 'a option t
  val afd : string -> 'a -> (string * 'a) list -> 'a t
  val elt : string -> 'a t -> 'a node
  val pcdata : string node
  val raw : xml node
  val wrap : 'a node -> ('a -> 'b) -> 'b node
  val union : 'a node list -> 'a node
  val one : 'a node -> 'a t
  val opt : 'a node -> 'a option t
  val any : 'a node -> 'a list t
end

let print_stack fmt stack =
  match stack with
    | [] -> ()
    | _ ->
        ignore (List.fold_left
                  (fun indent (name, attrs) ->
                     fprintf fmt "%s<%s" indent name;
                     List.iter (fun (k, v) -> fprintf fmt " %s=\"%s\"" k v) attrs;
                     fprintf fmt ">\n";
                     indent ^ "  ") "" stack)

module Make(Xml : XML) =
struct
  type xml = Xml.t

  type 'a result =
    | Value of 'a
    | Error of stack * string

  type node_type =
    | Element of string
    | Pcdata
    | Any
    | Union of node_type list

  type 'a node = node_type * (Xml.t -> 'a result option)

  module M =
  struct
    type 'a t = Xml.t list -> attributes -> Xml.t list * 'a result
    let bind m f xmls attrs =
      let rest, result = m xmls attrs in
        match result with
          | Value v -> f v rest attrs
          | Error _ as e -> (rest, e)
    let return v xmls attrs = (xmls, Value v)
  end

  include M
  module MaybeM = Util.MaybeT(M)

  let failwith msg xmls attrs = (xmls, Error([], msg))

  let (>>=) = bind

  let ao name xmls attrs =
    (xmls, Value (Util.assoc name attrs))

  let ar name =
    ao name >>= (function
                   | Some v -> return v
                   | None -> failwith (sprintf "attribute '%s' missing" name))

  let ad name default =
    ao name >>= (function
                   | Some v -> return v
                   | None -> return default)

  let af name field value = match Util.assoc value field with
    | Some v -> return v
    | None -> failwith (sprintf "unexpected value for '%s' (%s), must be one of %s"
                          name value (String.concat ", " (List.map fst field)))

  let afr name field = ar name >>= af name field
  let afo name field = MaybeM.bind (ao name) (fun v -> af name field v >>= MaybeM.return)

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
                               Xml.match_node ~element:(fun n _ _ -> n) ~pcdata:(fun _ -> "<pcdata>") x)
    with
        exn -> Error([], Printexc.to_string exn)

  let elt name elt_parser =
    (Element name,
     Xml.match_node
       ~element:(fun name' attrs children ->
                   if name = name'
                   then Some(match execute elt_parser children attrs with
                               | Error(stack, error) ->
                                   Error((name, attrs) :: stack, error)
                               | x -> x)
                   else None)
       ~pcdata:(fun _ -> None))

  let pcdata =
    (Pcdata,
     Xml.match_node
       ~element:(fun _ _ _ -> None)
       ~pcdata:(fun x -> Some(Value x)))

  let raw =
    (Any, (fun x -> Some(Value x)))

  let union nodes =
    let types, fl = List.split nodes in
      (Union types, fun node -> Util.find_map (fun f -> f node) fl)

  let wrap (typ, f) g = (typ, fun node ->
                           Util.Maybe.wrap
                             (function
                                | Value v -> Value (g v)
                                | Error _ as e -> e)
                             (f node))

  let string_of_type typ =
    let rec flat acc = function
      | Union l -> List.fold_left flat acc l
      | Pcdata -> "<pcdata>" :: acc
      | Any -> "<any>" :: acc
      | Element name -> name :: acc
    in
      match flat [] typ with
        | [] -> "<nothing>"
        | [x] -> x
        | l -> String.concat " or " l

  let opt (typ, f) xmls attrs =
    match Util.part_map f xmls with
      | [], rest -> (rest, Value None)
      | [x], rest -> (rest, Value None)
      | _, rest -> (rest, Error([], "too many nodes of type: " ^ string_of_type typ))

  let one (typ, f) =
    opt (typ, f) >>= function
      | Some x -> return x
      | None -> failwith ("element missing: " ^ string_of_type typ)

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
      | Value x -> `Right x
      | Error(stack, msg) -> `Left(stack, msg)
end
