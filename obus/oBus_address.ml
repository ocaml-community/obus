(*
 * oBus_address.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type name = string
type key = string
type value = string
type guid = OBus_uuid.t

type family = Ipv4 | Ipv6

type desc =
  | Unix_path of string
  | Unix_abstract of string
  | Unix_tmpdir of string
  | Tcp of string * string * family option
  | Autolaunch
  | Unknown of name * (key * value) list

type t = desc * guid option

exception Parse_failure of string

let assoc key default list = match Util.assoc key list with
  | Some v -> v
  | None -> default

let of_string str =
  try
    let buf = Buffer.create 42 in
    let addresses = List.rev (Addr_lexer.addresses [] buf (Lexing.from_string str)) in
      List.map begin fun (name, params) ->
        (begin match name with
           | "unix" -> begin
               match (Util.assoc "path" params,
                      Util.assoc "abstract" params,
                      Util.assoc "tmpdir" params) with
                 | Some path, None, None -> Unix_path path
                 | None, Some abst, None -> Unix_abstract abst
                 | None, None, Some tmpd -> Unix_tmpdir tmpd
                 | _ ->
                     ERROR("invalid unix address: must specify exactly one of \"path\", \"abstract\" or \"tmpdir\"");
                     Unknown(name, params)
             end
           | "tcp" ->
               let host = assoc "host" "" params
               and port = assoc "port" "0" params in
               begin match Util.assoc "family" params with
                 | Some "ipv4" -> Tcp(host, port, Some Ipv4)
                 | Some "ipv6" -> Tcp(host, port, Some Ipv6)
                 | Some f ->
                     ERROR("unknown address family: %S" f);
                     Unknown(name, params)
                 | None -> Tcp(host, port, None)
               end
           | "autolaunch" -> Autolaunch
           | _ -> Unknown(name, params)
         end,
         match Util.assoc "guid" params with
           | Some(guid_hex_encoded) -> Some(OBus_uuid.of_string guid_hex_encoded)
           | None -> None) end addresses
  with
      Failure msg ->
        DEBUG("failed to parse address %S: %s" str msg);
        raise (Parse_failure msg)

let system_bus_variable = "DBUS_SYSTEM_BUS_ADDRESS"
let session_bus_variable = "DBUS_SESSION_BUS_ADDRESS"
let session_bus_property = "_DBUS_SESSION_BUS_ADDRESS"

let default_session_bus_addresses = [Autolaunch, None]
let default_system_bus_addresses = [Unix_path "/var/run/dbus/system_bus_socket", None]

let system = lazy(
  match
    try Some (Sys.getenv system_bus_variable) with
        Not_found ->
          DEBUG("environment variable %s not found, using internal default" system_bus_variable);
          None
  with
    | Some str -> of_string str
    | None -> default_system_bus_addresses
)

let session = lazy(
  match
    try Some (Sys.getenv session_bus_variable) with
        Not_found ->
          LOG("environment variable %s not found" session_bus_variable);
          try
            (* Try with the root window property, this is bit ugly
               and it depends on the presence of xprop... *)
            Util.with_process_in (Printf.sprintf "xprop -root %s" session_bus_property)
              (fun ic -> Scanf.fscanf ic ("_DBUS_SESSION_BUS_ADDRESS(STRING) = %S") (fun s -> Some s))
          with
              exn ->
                LOG("can not get session bus address from property %s \
                     on root window (maybe x11 is not running), error is: %s"
                      session_bus_property (Printexc.to_string exn));
                None
  with
    | Some str -> of_string str
    | None -> default_session_bus_addresses
)
