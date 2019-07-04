(*
 * oBus_match.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(match)"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type argument_filter =
  | AF_string of string
  | AF_string_path of string
  | AF_namespace of string

type arguments = (int * argument_filter) list

type rule = {
  typ : [ `Signal | `Error | `Method_call | `Method_return ] option;
  sender : OBus_name.bus;
  interface : OBus_name.interface;
  member : OBus_name.member;
  path : OBus_path.t option;
  destination : OBus_name.bus;
  arguments : arguments;
  eavesdrop : bool option;
}

let typ e = e.typ
let sender e = e.sender
let interface e = e.interface
let member e = e.member
let path e = e.path
let destination e = e.destination
let arguments e = e.arguments
let eavesdrop e = e.eavesdrop

let rule ?typ ?(sender="") ?(interface="") ?(member="") ?path ?(destination="") ?(arguments=[]) ?eavesdrop () = {
  typ = typ;
  sender = sender;
  interface = interface;
  member = member;
  path = path;
  destination = destination;
  arguments = arguments;
  eavesdrop = eavesdrop;
}

(* +-----------------------------------------------------------------+
   | Arguments lists                                                 |
   +-----------------------------------------------------------------+ *)

let rec insert_sorted num filter = function
  | [] -> [(num, filter)]
  | (num', _) as pair :: rest when num' < num ->
      pair :: insert_sorted num filter rest
  | (num', _) :: rest when num' = num ->
      (num, filter) :: rest
  | ((num', _) :: rest) as l ->
      (num, filter) :: l

let make_arguments list =
  List.fold_left
    (fun l (num, filter) ->
       if num < 0 || num > 63 then
         Printf.ksprintf invalid_arg "OBus_match.arguments_of_list: invalid argument number '%d': it must be in the rane [1..63]" num
       else
         insert_sorted num filter l)
    [] list

external cast_arguments : arguments -> (int * argument_filter) list = "%identity"

(* +-----------------------------------------------------------------+
   | string <-> rule                                                 |
   +-----------------------------------------------------------------+ *)

let string_of_rule mr =
  let buf = Buffer.create 42 in
  let rec coma = ref (fun _ -> coma := fun _ -> Buffer.add_char buf ',') in
  let add key value =
    !coma ();
    Buffer.add_string buf key;
    Buffer.add_string buf "='";
    Buffer.add_string buf value;
    Buffer.add_char buf '\''
  in
  let add_string key test = function
    | "" -> ()
    | str ->
        match test str with
          | Some error -> raise (OBus_string.Invalid_string error)
          | None -> add key str
  in
  begin
    match mr.typ with
      | None -> ()
      | Some t ->
          add "type"
            (match t with
               | `Method_call -> "method_call"
               | `Method_return -> "method_return"
               | `Error -> "error"
               | `Signal -> "signal")
  end;
  add_string "sender" OBus_name.validate_bus mr.sender;
  add_string "interface" OBus_name.validate_interface mr.interface;
  add_string "member" OBus_name.validate_member mr.member;
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
  add_string "destination" OBus_name.validate_bus mr.destination;
  List.iter (fun (n, filter) ->
               !coma ();
               match filter with
                 | AF_string str ->
                     Printf.bprintf buf "arg%d='%s'" n str
                 | AF_string_path str ->
                     Printf.bprintf buf "arg%dpath='%s'" n str
                 | AF_namespace str ->
                     Printf.bprintf buf "arg%dnamespace='%s'" n str) mr.arguments;
  begin match mr.eavesdrop with
    | None -> ()
    | Some true -> add "eavesdrop" "true"
    | Some false -> add "eavesdrop" "false"
  end;
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
      sender = "";
      interface = "";
      member = "";
      path = None;
      destination = "";
      arguments = [];
      eavesdrop = None;
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
            { mr with sender = value }
        | "destination" ->
            check pos OBus_name.validate_bus value;
            { mr with destination = value }
        | "interface" ->
            check pos OBus_name.validate_interface value;
            { mr with interface = value }
        | "member" ->
            check pos OBus_name.validate_member value;
            { mr with member = value }
        | "path" -> begin
            try
              { mr with path = Some(OBus_path.of_string value) }
            with OBus_string.Invalid_string err ->
              raise (Fail(pos, OBus_string.error_message err))
          end
        | "eavesdrop" -> begin
            match value with
              | "true" -> { mr with eavesdrop = Some true }
              | "false" -> { mr with eavesdrop = Some false }
              | _ -> raise (Fail(pos, Printf.sprintf "invalid value for eavesdrop (%s)" value))
          end
        | _ ->
            match OBus_match_rule_lexer.arg (Lexing.from_string key) with
              | Some(n, kind) ->
                  { mr with arguments =
                      insert_sorted n
                        (match kind with
                           | `String -> AF_string value
                           | `Path -> AF_string_path value
                           | `Namespace -> AF_namespace value)
                        mr.arguments }
              | None ->
                  raise (Fail(pos, Printf.sprintf "invalid key (%s)" key))
    end mr l
  with Fail(pos, msg) ->
    raise (Parse_failure(str, pos, msg))

(* +-----------------------------------------------------------------+
   | Matching                                                        |
   +-----------------------------------------------------------------+ *)

let match_key matcher value = match matcher with
  | None -> true
  | Some value' -> value = value'

let match_string matcher value = match matcher with
  | "" -> true
  | value' -> value = value'

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
  | OBus_value.V.Basic(OBus_value.V.String value) :: rest ->
      (match filter with
         | AF_string str ->
             str = value
         | AF_string_path str ->
             (str = value)
             || (ends_with_slash str && starts_with value str)
             || (ends_with_slash value && starts_with str value)
         | AF_namespace str ->
             starts_with value str &&
               (String.length value = String.length str ||
                   value.[String.length str] = '.'))
      &&  match_arguments (num + 1) matcher rest
  | OBus_value.V.Basic(OBus_value.V.Object_path value) :: rest ->
      (match filter with
         | AF_string str ->
             false
         | AF_string_path str ->
             let value = OBus_path.to_string value in
             (str = value)
             || (ends_with_slash str && starts_with value str)
             || (ends_with_slash value && starts_with str value)
         | AF_namespace _ ->
             false)
      &&  match_arguments (num + 1) matcher rest
  | _ ->
      false

let match_values filters values =
  match_arguments 0 filters values

let match_message mr msg =
  (match OBus_message.typ msg, mr.typ with
     | OBus_message.Method_call(path, interface, member), (Some `Method_call | None) ->
         (match_key mr.path path) &&
           (match_string mr.interface interface) &&
           (match_string mr.member member)
     | OBus_message.Method_return serial, (Some `Method_return | None)->
         true
     | OBus_message.Signal(path, interface, member), (Some `Signal | None) ->
         (match_key mr.path path) &&
           (match_string mr.interface interface) &&
           (match_string mr.member member)
     | OBus_message.Error(serial, name), (Some `Error | None) ->
         true
     | _ ->
         false) &&
    (match_string mr.sender (OBus_message.sender msg)) &&
    (match_string mr.destination (OBus_message.destination msg)) &&
    (match_arguments 0 mr.arguments (OBus_message.body msg))

(* +-----------------------------------------------------------------+
   | Comparison                                                      |
   +-----------------------------------------------------------------+ *)

type comparison_result =
  | More_general
  | Less_general
  | Equal
  | Incomparable

let rec compare_arguments acc l1 l2 =
  match acc, l1, l2 with
    | acc, [], [] ->
        acc
    | (Less_general | Equal), _ :: _, [] ->
        Less_general
    | (More_general | Equal), [], _ :: _ ->
        More_general
    | acc, (pos1, filter1) :: rest1, (pos2, filter2) :: rest2 ->
        if pos1 = pos2 && filter1 = filter2 then
          compare_arguments acc rest1 rest2
        else if pos1 < pos2 && (acc = Less_general || acc = Equal) then
          compare_arguments Less_general rest1 l2
        else if pos1 > pos2 && (acc = More_general || acc = Equal) then
          compare_arguments More_general l1 rest2
        else
          raise Exit
    | _ ->
        raise Exit

let compare_option acc x1 x2 =
  if x1 = x2 then
    acc
  else
    match acc, x1, x2 with
      | (Less_general | Equal), Some _, None ->
          Less_general
      | (More_general | Equal), None, Some _ ->
          More_general
      | _ ->
          raise Exit

let compare_string acc x1 x2 =
  if x1 = x2 then
    acc
  else
    match acc, x1, x2 with
      | (Less_general | Equal), x, "" when x <> "" ->
          Less_general
      | (More_general | Equal), "", x when x <> "" ->
          More_general
      | _ ->
          raise Exit

let compare_rules r1 r2 =
  try
    if r1.typ = r2.typ then begin
      let acc = Equal in
      let acc = compare_string acc r1.sender r2.sender in
      let acc = compare_string acc r1.destination r2.destination in
      let acc = compare_option acc r1.path r2.path in
      let acc = compare_string acc r1.interface r2.interface in
      let acc = compare_string acc r1.member r2.member in
      let acc = compare_arguments acc r1.arguments r2.arguments in
      if r1.eavesdrop = r2.eavesdrop then
        acc
      else
        match acc, r1.eavesdrop, r2.eavesdrop with
          | _, None, Some false ->
              acc
          | _, Some false, None ->
              acc
          | (Less_general | Equal), (None | Some false), Some true ->
              Less_general
          | (More_general | Equal), Some true, (None | Some false) ->
              More_general
          | _ ->
              Incomparable
    end else
      Incomparable
  with Exit ->
    Incomparable

(* +-----------------------------------------------------------------+
   | Exporting rules on message buses                                |
   +-----------------------------------------------------------------+ *)

module String_set = Set.Make(String)

(* Informations stored in connections *)
type info = {
  mutable exported : String_set.t;
  (* Rules that are currently exported on the message bus (as strings) *)

  mutable rules : rule list;
  (* The list of all rules we want to export *)

  connection : OBus_connection.t;
  (* The connection on which the rules are exported *)

  mutex : Lwt_mutex.t;
  (* Mutex to prevent concurrent modifications of rules *)
}

(* Add a matching rule to a list of incomparable most general rules *)
let rec insert_rule rule rules =
  match rules with
    | [] ->
        [rule]
    | rule' :: rest ->
        match compare_rules rule rule' with
          | Incomparable ->
              rule' :: insert_rule rule rest
          | Equal | Less_general ->
              rules
          | More_general ->
              rule :: rest

let do_export info rule_string =
  let%lwt () =
    OBus_connection.method_call
      ~connection:info.connection
      ~destination:OBus_protocol.bus_name
      ~path:OBus_protocol.bus_path
      ~interface:OBus_protocol.bus_interface
      ~member:"AddMatch"
      ~i_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
      ~o_args:OBus_value.C.seq0
      rule_string
  in
  info.exported <- String_set.add rule_string info.exported;
  Lwt.return ()

let do_remove info rule_string =
  info.exported <- String_set.remove rule_string info.exported;
  try%lwt
    OBus_connection.method_call
      ~connection:info.connection
      ~destination:OBus_protocol.bus_name
      ~path:OBus_protocol.bus_path
      ~interface:OBus_protocol.bus_interface
      ~member:"RemoveMatch"
      ~i_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
      ~o_args:OBus_value.C.seq0
      rule_string
  with exn ->
    match OBus_error.name exn with
      | "org.freedesktop.DBus.Error.MatchRuleNotFound" ->
          Lwt_log.info_f ~section "rule %S does not exists on the message bus" rule_string
      | _ ->
          Lwt.fail exn

(* Commits rules changes on the message bus: *)
let commit info =
  Lwt_mutex.with_lock info.mutex
    (fun () ->
       (* Computes the set of most general rules: *)
       let rules = List.fold_left (fun acc rule -> insert_rule rule acc) [] info.rules in

       (* Turns them into a set of strings: *)
       let rules = List.fold_left (fun acc rule -> String_set.add (string_of_rule rule) acc) String_set.empty rules in

       (* Computes the minimal set of operations to update the rules: *)
       let new_rules = String_set.diff rules info.exported
       and old_rules = String_set.diff info.exported rules in

       (* Does the update of rules on the message bus: *)
       let threads = [] in
       let threads = String_set.fold (fun rule acc -> do_export info rule :: acc) new_rules threads in
       let threads = String_set.fold (fun rule acc -> do_remove info rule :: acc) old_rules threads in

       Lwt.join threads)

let key = OBus_connection.new_key ()

let rec remove_first x l =
  match l with
    | [] -> []
    | x' :: l when x = x' -> l
    | x' :: l -> x' :: remove_first x l

let export ?switch connection rule =
  Lwt_switch.check switch;
  let info =
    match OBus_connection.get connection key with
      | Some info ->
          info
      | None ->
          let info = {
            exported = String_set.empty;
            connection = connection;
            rules = [];
            mutex = Lwt_mutex.create ();
          } in
          OBus_connection.set connection key (Some info);
          info
  in
  info.rules <- rule :: info.rules;
  let%lwt () = commit info in
  let%lwt () =
    Lwt_switch.add_hook_or_exec switch
      (fun () ->
         info.rules <- remove_first rule info.rules;
         commit info)
  in
  Lwt.return ()
