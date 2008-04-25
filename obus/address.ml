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

type t =
  | Unix of string
      (** A unix socket, the argument is the path *)
  | Unknown of name * (key * value) list
      (** Unknown address *)

exception Parse_error of string

let of_string str =
  try
    let buf = Buffer.create 42 in
    let addresses = List.rev (AddrLexer.addresses [] buf) in
      List.map (fun (name, params) -> match name with
                  | "unix" -> begin
                      match Util.assoc "path" params, Util.assoc "abstract" params with
                        | Some path, None -> Unix path
                        | None, Some abst -> Unix ("\x00" ^ abst)
                        | _ -> Unknown(name, params)
                    end
                      (* XXX TODO: handle more addresses and transport
                         (tcp, ...) XXX *)
                  | _ -> None) addresses
  with
      Failure msg -> raise (Parse_error msg)

let system () =
  of_string
    (try Unix.getenv "DBUS_SYSTEM_BUS_ADDRESS" with
         Not_found -> Constant.default_system_bus_address)

let session () =
  try of_string (Unix.getenv "DBUS_SESSION_BUS_ADDRESS") with
      Not_found -> []
