(*
 * address.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type name = string
type key = string
type value = string
type guid = string

type raw = name * (key * value) list

type known =
  | Unix of string
  | Unknown

type t = raw * known * guid option

exception Parse_error of string

let of_string str =
  try
    let buf = Buffer.create 42 in
    let addresses = List.rev (AddrLexer.addresses [] buf (Lexing.from_string str)) in
      List.map begin fun ((name, params) as raw) ->
        (raw,
         begin match name with
           | "unix" -> begin
               match Util.assoc "path" params, Util.assoc "abstract" params with
                 | Some path, None -> Unix path
                 | None, Some abst -> Unix ("\x00" ^ abst)
                 | None, None ->
                     ERROR("invalid unix address: can not specify \"path\" and \"abstract\" at the same time");
                     Unknown
                 | Some _, Some _ ->
                     ERROR("invalid unix address: must specify \"path\" or \"abstract\"");
                     Unknown
             end
               (* XXX TODO: handle more addresses and transport
                  (tcp, ...) XXX *)
           | _ -> Unknown
         end,
         match Util.assoc "guid" params with
           | Some(guid_hex_encoded) ->
               let lexbuf = Lexing.from_string guid_hex_encoded in
                 Buffer.clear buf;
                 for i = 1 to (String.length guid_hex_encoded) / 2 do
                   AddrLexer.unescape_char buf lexbuf
                 done;
                 Some(Buffer.contents buf)
           | None -> None) end addresses
  with
      Failure msg -> raise (Parse_error msg)

let system_bus_variable = "DBUS_SYSTEM_BUS_ADDRESS"
let session_bus_variable = "DBUS_SESSION_BUS_ADDRESS"
let session_bus_property = "_DBUS_SESSION_BUS_ADDRESS"

let system () =
  of_string
    (try Sys.getenv system_bus_variable with
         Not_found ->
           DEBUG("environment variable %s not found, using internal default" system_bus_variable);
           Constant.default_system_bus_address)

let session () =
  match
    try Some (Sys.getenv session_bus_variable) with
        Not_found ->
          LOG("environment variable %s not found" session_bus_variable);
          try
            (* Try with the root window property, this is bit ugly and
               it depends on the presence of xprop... *)
            let ic = Unix.open_process_in
              (Printf.sprintf "xprop -root %s" session_bus_property)
            in
            let result = try
              Scanf.fscanf ic ("_DBUS_SESSION_BUS_ADDRESS(STRING) = %S") (fun s -> Some s)
            with
                _ -> None
            in
              ignore (Unix.close_process_in ic);
              result
          with
              _ ->
                LOG("can not get session bus address from property %s on root window (maybe x11 is not running)"
                      session_bus_property);
                None
  with
    | Some str -> of_string str
    | None -> []
