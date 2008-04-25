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

let system () =
  of_string
    (try Sys.getenv "DBUS_SYSTEM_BUS_ADDRESS" with
         Not_found -> Constant.default_system_bus_address)

let session () =
  try of_string (Sys.getenv "DBUS_SESSION_BUS_ADDRESS") with
      Not_found -> []
