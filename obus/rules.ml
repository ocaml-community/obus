(*
 * rules.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let to_string ?typ ?sender ?interface ?member ?path ?destination ?(args=[]) () =
  let buf = Buffer.create 42 in
  let first = ref true in
  let coma () =
    if !first
    then first := false
    else Buffer.add_char buf ',' in
  let add key value =
    coma ();
    Printf.bprintf buf "%s='%s'" key value in
  let add_opt key validator = function
    | None -> ()
    | Some x -> validator x; add key x in
  begin match typ with
    | None -> ()
    | Some t ->
        add "type"
          (match t with
             | `method_call -> "method_call"
             | `method_return -> "method_return"
             | `error -> "error"
             | `signal -> "signal")
  end;
  add_opt "sender" OBus_name.Connection.validate sender;
  add_opt "interface" OBus_name.Interface.validate interface;
  add_opt "member" OBus_name.Member.validate member;
  add_opt "path" OBus_path.validate path;
  add_opt "destination" OBus_name.Connection_unique.validate destination;
  List.iter (fun (n, value) -> coma (); Printf.bprintf buf "arg%d='%s'" n value) args;
  Buffer.contents buf
