(*
 * oBus_address.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Log = Lwt_log.Make(struct let section = "obus(address)" end)

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type guid = OBus_uuid.t

type t = {
  name : string;
  args : (string * string) list;
} with projection

let make ~name ~args = { name = name; args = args }

let arg arg address =
  OBus_util.assoc arg address.args

let guid address =
  match OBus_util.assoc "guid" address.args with
    | Some guid -> Some(OBus_uuid.of_string guid)
    | None -> None

(* +-----------------------------------------------------------------+
   | Parsing/marshaling                                              |
   +-----------------------------------------------------------------+ *)

exception Parse_failure of string * int * string

let () =
  Printexc.register_printer
    (function
       | Parse_failure(str, pos, msg) ->
           Some(Printf.sprintf "failed to parse D-Bus addresses %S, at position %d: %s" str pos msg)
       | _ ->
           None)

let of_string str =
  try
    List.map
      (fun (name, args) -> { name = name; args = args })
      (OBus_address_lexer.addresses (Lexing.from_string str))
  with OBus_address_lexer.Fail(pos, msg) ->
    raise (Parse_failure(str, pos, msg))

let to_string l =
  let buf = Buffer.create 42 in
  let escape = String.iter begin fun ch -> match ch with
    | '0'..'9' | 'A'..'Z' | 'a'..'z'
    | '_' | '-' | '/' | '.' | '\\' ->
        Buffer.add_char buf ch
    | _ ->
        Printf.bprintf buf "%%%02x" (Char.code ch)
  end in
  let concat ch f = function
    | [] -> ()
    | x :: l -> f x; List.iter (fun x -> Buffer.add_char buf ch; f x) l
  in
  concat ';' begin fun { name = name; args = args } ->
    Buffer.add_string buf name;
    Buffer.add_char buf ':';
    concat ','
      (fun (k, v) ->
         Buffer.add_string buf k;
         Buffer.add_char buf '=';
         escape v)
      args
  end l;
  Buffer.contents buf

let obus_list = OBus_type.map OBus_private_type.obus_string of_string to_string

(* +-----------------------------------------------------------------+
   | Well known addresses                                            |
   +-----------------------------------------------------------------+ *)

let system_bus_variable = "DBUS_SYSTEM_BUS_ADDRESS"
let session_bus_variable = "DBUS_SESSION_BUS_ADDRESS"

let default_system = [{ name = "unix"; args = [("path", "/var/run/dbus/system_bus_socket")] }]
let default_session = [{ name = "autolaunch"; args = [] }]

open Lwt

let not_found_msg : (_, _, _, _) format4 = "environment variable %s not found, using internal default"

let system = lazy(
  match try Some (Sys.getenv system_bus_variable) with Not_found -> None with
    | Some str ->
        return (of_string str)
    | None ->
        lwt () = Log.info_f not_found_msg system_bus_variable in
        return default_system
)

let session = lazy(
  match try Some(Sys.getenv session_bus_variable) with Not_found -> None with
    | Some line ->
        return (of_string line)
    | None ->
        lwt () = Log.info_f not_found_msg session_bus_variable in
        return default_session
)
