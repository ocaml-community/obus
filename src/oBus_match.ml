(*
 * oBus_match.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type rule = {
  typ : [ `signal | `error | `method_call | `method_return ] option;
  sender : OBus_name.bus option;
  interface : OBus_name.interface option;
  member : OBus_name.member option;
  path : OBus_path.t option;
  destination : OBus_name.bus option;
  arguments : (int * string) list;
} with projection

let rec insert_sorted num value = function
  | [] -> [(num, value)]
  | (num', _) as pair :: rest when num' < num ->
      pair :: insert_sorted num value rest
  | (num', _) :: rest when num' = num ->
      (num, value) :: rest
  | ((num', _) :: rest) as l ->
      (num, value) :: l

let rule ?typ ?sender ?interface ?member ?path ?destination ?(arguments=[]) () = {
  typ = typ;
  sender = sender;
  interface = interface;
  member = member;
  path = path;
  destination = destination;
  arguments = List.fold_left (fun l (num, value) -> insert_sorted num value l) [] arguments;
}

let string_of_rule mr =
  let buf = Buffer.create 42 in
  let rec coma = ref (fun _ -> coma := fun _ -> Buffer.add_char buf ',') in
  let add key value =
    !coma ();
    Buffer.add_string buf key;
    Buffer.add_string buf "='";
    Buffer.add_string buf value;
    Buffer.add_char buf '\'' in
  let add_opt key test = function
    | None -> ()
    | Some x -> match test x with
        | Some error -> raise (OBus_string.Invalid_string error)
        | None -> add key x in
  begin match mr.typ with
    | None -> ()
    | Some t ->
        add "type"
          (match t with
             | `method_call -> "method_call"
             | `method_return -> "method_return"
             | `error -> "error"
             | `signal -> "signal")
  end;
  add_opt "sender" OBus_name.validate_bus mr.sender;
  add_opt "interface" OBus_name.validate_interface mr.interface;
  add_opt "member" OBus_name.validate_member mr.member;
  begin match mr.path with
    | None -> ()
    | Some [] ->
        !coma ();
        Buffer.add_string buf "path='/'"
    | Some p ->
        !coma ();
        Buffer.add_string buf "path='";
        List.iter
          (fun elt ->
             match OBus_path.validate_element elt with
               | Some error ->
                   raise (OBus_string.Invalid_string error)
               | None ->
                   Buffer.add_char buf '/';
                   Buffer.add_string buf elt)
          p;
        Buffer.add_char buf '\''
  end;
  add_opt "destination" OBus_name.validate_bus mr.destination;
  List.iter (fun (n, value) -> !coma (); Printf.bprintf buf "arg%d='%s'" n value) mr.arguments;
  Buffer.contents buf

let rule_of_string str =
  let l =
    try
      OBus_match_rule_lexer.match_rules (Lexing.from_string str)
    with exn ->
      failwith "OBus_match.rule_of_string: invalid matching rule"
  in
  let check validate value =
    match validate value with
      | None ->
          ()
      | Some err ->
          raise (OBus_string.Invalid_string err)
  in
  let mr = {
    typ = None;
    sender = None;
    interface = None;
    member = None;
    path = None;
    destination = None;
    arguments = [];
  } in
  List.fold_left begin fun mr (key, value) ->
    match key with
      | "type" ->
          { mr with typ = Some(match value with
                                 | "method_call" -> `method_call
                                 | "method_return" -> `method_return
                                 | "signal" -> `signal
                                 | "error" -> `error
                                 | _ -> Printf.ksprintf failwith "OBus_match.rule_of_string: invalid type (%s)" value) }
      | "sender" ->
          check OBus_name.validate_bus value;
          { mr with sender = Some value }
      | "destination" ->
          check OBus_name.validate_bus value;
          { mr with destination = Some value }
      | "interface" ->
          check OBus_name.validate_interface value;
          { mr with interface = Some value }
      | "member" ->
          check OBus_name.validate_member value;
          { mr with member = Some value }
      | "path" ->
          { mr with path = Some(OBus_path.of_string value) }
      | _ ->
          match OBus_match_rule_lexer.arg (Lexing.from_string key) with
            | Some n ->
                if n < 64 then
                  { mr with arguments = insert_sorted n value mr.arguments }
                else
                  Printf.ksprintf failwith "OBus_match.rule_of_string: invalid argument number (%d)" n
            | None ->
                Printf.ksprintf failwith "OBus_match.rule_of_string: invalid key (%s)" key
  end mr l

let obus_rule = OBus_type.map OBus_type.Pervasives.obus_string rule_of_string string_of_rule

let match_key matcher value = match matcher with
  | None -> true
  | Some value' -> value = value'

let rec match_arguments num matcher arguments = match matcher with
  | [] ->
      true
  | (num', value') :: rest ->
      match_arguments_aux num num' value' rest arguments

and match_arguments_aux num num' value' matcher arguments = match arguments with
  | [] ->
      false
  | value :: rest when num < num' ->
      match_arguments_aux (num + 1) num' value' matcher rest
  | OBus_value.Basic(OBus_value.String value) :: rest ->
      value = value' && match_arguments (num + 1) matcher rest
  | _ ->
      false

let match_message mr msg =
  (match OBus_message.typ msg, mr.typ with
     | OBus_message.Method_call(path, interface, member), (Some `method_call | None) ->
         (match_key mr.path path) &&
           (mr.interface = None || mr.interface = interface) &&
           (match_key mr.member member)
     | OBus_message.Method_return serial, (Some `method_return | None)->
         true
     | OBus_message.Signal(path, interface, member), (Some `signal | None) ->
         (match_key mr.path path) &&
           (match_key mr.interface interface) &&
           (match_key mr.member member)
     | OBus_message.Error(serial, name), (Some `error | None) ->
         true
     | _ ->
         false) &&
    (mr.sender = None || mr.sender = OBus_message.sender msg) &&
    (mr.destination = None || mr.destination = OBus_message.destination msg) &&
    (match_arguments 0 mr.arguments (OBus_message.body msg))
