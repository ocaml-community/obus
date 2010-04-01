(*
 * oBus_match.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type argument_filter =
  | AF_string of string
  | AF_string_path of string

type rule = {
  typ : [ `Signal | `Error | `Method_call | `Method_return ] option;
  sender : OBus_name.bus option;
  interface : OBus_name.interface option;
  member : OBus_name.member option;
  path : OBus_path.t option;
  destination : OBus_name.bus option;
  arguments : (int * argument_filter) list;
} with projection

let rec insert_sorted num filter = function
  | [] -> [(num, filter)]
  | (num', _) as pair :: rest when num' < num ->
      pair :: insert_sorted num filter rest
  | (num', _) :: rest when num' = num ->
      (num, filter) :: rest
  | ((num', _) :: rest) as l ->
      (num, filter) :: l

let rule ?typ ?sender ?interface ?member ?path ?destination ?(arguments=[]) () = {
  typ = typ;
  sender = sender;
  interface = interface;
  member = member;
  path = path;
  destination = destination;
  arguments = List.fold_left (fun l (num, filter) ->
                                if num < 0 || num > 63 then
                                  Printf.ksprintf invalid_arg "OBus_match.rule: invalid argument number '%d': it must be in the rane [1..63]" num
                                else
                                  insert_sorted num filter l) [] arguments;
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
             | `Method_call -> "method_call"
             | `Method_return -> "method_return"
             | `Error -> "error"
             | `Signal -> "signal")
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
  List.iter (fun (n, filter) ->
               !coma ();
               match filter with
                 | AF_string str ->
                     Printf.bprintf buf "arg%d='%s'" n str
                 | AF_string_path str ->
                     Printf.bprintf buf "arg%dpath='%s'" n str) mr.arguments;
  Buffer.contents buf

exception Parse_failure of string * int * string

let () =
  Printexc.register_printer
    (function
       | Parse_failure(str, pos, reason) ->
           Some(Printf.sprintf "failed to parse D-Bus matching rule %S, at position %d: %s" str pos reason)
       | _ ->
           None)

exception Fail = OBus_match_rule_lexer.Fail

let rule_of_string str =
  try
    let l = match str with
      | "" -> []
      | _ -> OBus_match_rule_lexer.match_rules (Lexing.from_string str)
    in
    let check pos validate value =
      match validate value with
        | None ->
            ()
        | Some err ->
            raise (Fail(pos, OBus_string.error_message err))
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
    List.fold_left begin fun mr (pos, key, value) ->
      match key with
        | "type" ->
            { mr with typ = Some(match value with
                                   | "method_call" -> `Method_call
                                   | "method_return" -> `Method_return
                                   | "signal" -> `Signal
                                   | "error" -> `Error
                                   | _ -> raise (Fail(pos, Printf.sprintf "invalid message type (%s)" value))) }
        | "sender" ->
            check pos OBus_name.validate_bus value;
            { mr with sender = Some value }
        | "destination" ->
            check pos OBus_name.validate_bus value;
            { mr with destination = Some value }
        | "interface" ->
            check pos OBus_name.validate_interface value;
            { mr with interface = Some value }
        | "member" ->
            check pos OBus_name.validate_member value;
            { mr with member = Some value }
        | "path" -> begin
            try
              { mr with path = Some(OBus_path.of_string value) }
            with OBus_string.Invalid_string err ->
              raise (Fail(pos, OBus_string.error_message err))
          end
        | _ ->
            match OBus_match_rule_lexer.arg (Lexing.from_string key) with
              | Some(n, is_path) ->
                  { mr with arguments = insert_sorted n (if is_path then AF_string_path value else AF_string value)  mr.arguments }
              | None ->
                  raise (Fail(pos, Printf.sprintf "invalid key (%s)" key))
    end mr l
  with Fail(pos, msg) ->
    raise (Parse_failure(str, pos, msg))

let obus_rule = OBus_type.map OBus_pervasives.obus_string rule_of_string string_of_rule

let match_key matcher value = match matcher with
  | None -> true
  | Some value' -> value = value'

let starts_with str prefix =
  let str_len = String.length str and prefix_len = String.length prefix in
  let rec loop i =
    (i = prefix_len) || (i < str_len && str.[i] = prefix.[i] && loop (i + 1))
  in
  loop 0

let ends_with_slash str = str <> "" && str.[String.length str - 1] = '/'

let rec match_arguments num matcher arguments = match matcher with
  | [] ->
      true
  | (num', filter) :: rest ->
      match_arguments_aux num num' filter rest arguments

and match_arguments_aux num num' filter matcher arguments = match arguments with
  | [] ->
      false
  | value :: rest when num < num' ->
      match_arguments_aux (num + 1) num' filter matcher rest
  | OBus_value.Basic(OBus_value.String value) :: rest ->
      (match filter with
         | AF_string str ->
             str = value
         | AF_string_path str ->
             (str = value)
             || (ends_with_slash str && starts_with value str)
             || (ends_with_slash value && starts_with str value))
      &&  match_arguments (num + 1) matcher rest
  | _ ->
      false

let match_message mr msg =
  (match OBus_message.typ msg, mr.typ with
     | OBus_message.Method_call(path, interface, member), (Some `Method_call | None) ->
         (match_key mr.path path) &&
           (mr.interface = None || mr.interface = interface) &&
           (match_key mr.member member)
     | OBus_message.Method_return serial, (Some `Method_return | None)->
         true
     | OBus_message.Signal(path, interface, member), (Some `Signal | None) ->
         (match_key mr.path path) &&
           (match_key mr.interface interface) &&
           (match_key mr.member member)
     | OBus_message.Error(serial, name), (Some `Error | None) ->
         true
     | _ ->
         false) &&
    (mr.sender = None || mr.sender = OBus_message.sender msg) &&
    (mr.destination = None || mr.destination = OBus_message.destination msg) &&
    (match_arguments 0 mr.arguments (OBus_message.body msg))
