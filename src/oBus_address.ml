(*
 * oBus_address.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type guid = OBus_uuid.t

type tcp_params = {
  tcp_host : string;
  tcp_bind : string;
  tcp_port : string;
  tcp_family : [ `Ipv4 | `Ipv6 ] option;
} with projection

type address =
  | Unix_path of string
  | Unix_abstract of string
  | Unix_tmpdir of string
  | Tcp of tcp_params
  | Autolaunch
  | Unknown of string * (string * string) list
 with constructor

type t = { address : address; guid : guid option }
 with projection

exception Parse_failure of string

let assoc key default list = match OBus_util.assoc key list with
  | Some v -> v
  | None -> default

let of_string str =
  try
    let buf = Buffer.create 42 in
    let addresses = List.rev (OBus_address_lexer.addresses [] buf (Lexing.from_string str)) in
    List.map begin fun (name, params) -> {
      address = (match name with
                   | "unix" -> begin
                       match (OBus_util.assoc "path" params,
                              OBus_util.assoc "abstract" params,
                              OBus_util.assoc "tmpdir" params) with
                         | Some path, None, None -> Unix_path path
                         | None, Some abst, None -> Unix_abstract abst
                         | None, None, Some tmpd -> Unix_tmpdir tmpd
                         | _ ->
                             ERROR("invalid unix address: must specify exactly one of \"path\", \"abstract\" or \"tmpdir\"");
                             Unknown(name, params)
                     end
                   | "tcp" ->
                       Tcp{ tcp_host = assoc "host" "" params;
                            tcp_port = assoc "port" "0" params;
                            tcp_bind = assoc "bind" "*" params;
                            tcp_family = match OBus_util.assoc "family" params with
                              | Some "ipv4" -> Some `Ipv4
                              | Some "ipv6" -> Some `Ipv4
                              | Some f ->
                                  ERROR("unknown address family: %S" f);
                                  None
                              | None -> None }
                   | "autolaunch" ->
                       Autolaunch
                   | _ ->
                       Unknown(name, params));
      guid = (match OBus_util.assoc "guid" params with
                | Some guid_hex_encoded -> Some(OBus_uuid.of_string guid_hex_encoded)
                | None -> None);
    } end addresses
  with Failure msg ->
    DEBUG("failed to parse address %S: %s" str msg);
    raise (Parse_failure msg)

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
  concat ':' begin fun { address = address; guid = guid } ->
    let name, params = match address with
      | Unix_path path ->
          "unix", [("path", path)]
      | Unix_abstract path ->
          "unix", [("abstract", path)]
      | Unix_tmpdir path ->
          "unix", [("tmpdir", path)]
      | Tcp { tcp_host = host;
              tcp_port = port;
              tcp_bind = bind;
              tcp_family = family } ->
          "tcp", (OBus_util.filter_map (fun x -> x)
                    [(if host <> "" then Some("host", host) else None);
                     (if bind <> "" && bind <> "*" then Some("bind", bind) else None);
                     (if port <> "0" then Some("port", port) else None);
                     (OBus_util.wrap_option family
                        (fun f -> ("family", match f with
                                     | `Ipv4 -> "ipv4"
                                     | `Ipv6 -> "ipv6")))])

      | Autolaunch ->
          "autolaunch", []
      | Unknown(name, params) ->
          name, params
    in
    Buffer.add_string buf name;
    Buffer.add_char buf ':';
    concat ',' (fun (k, v) ->
                  Buffer.add_string buf k;
                  Buffer.add_char buf '=';
                  escape v) (match guid with
                               | Some g -> params @ [("guid", OBus_uuid.to_string g)]
                               | None -> params)
  end l;
  Buffer.contents buf

let obus_list = OBus_type.map OBus_type.Perv.obus_string of_string to_string

let system_bus_variable = "DBUS_SYSTEM_BUS_ADDRESS"
let session_bus_variable = "DBUS_SESSION_BUS_ADDRESS"
let session_bus_property = "_DBUS_SESSION_BUS_ADDRESS"

let default_session_bus_addresses = { address = Autolaunch; guid = None }
let default_system_bus_addresses = { address = Unix_path "/var/run/dbus/system_bus_socket"; guid = None }

open Lwt

let system = lazy (
  match
    try Some (Sys.getenv system_bus_variable) with
        Not_found ->
          DEBUG("environment variable %s not found, using internal default" system_bus_variable);
          None
  with
    | Some str -> try_lwt return (of_string str)
    | None -> return [default_system_bus_addresses]
)

let session = lazy (
  match try Some(Sys.getenv session_bus_variable) with Not_found -> None with
    | Some line ->
        try_lwt return (of_string line)
    | None ->
        LOG("environment variable %s not found" session_bus_variable);
        try_lwt
          (* Try with the root window property, this is bit ugly and
             it depends on the presence of xprop... *)
          lwt line = Lwt_process.pread_line ("xprop", [|"xprop"; "-root"; session_bus_property|]) in
          Scanf.sscanf line "_DBUS_SESSION_BUS_ADDRESS(STRING) = %S" (fun x -> try_lwt return (of_string x))
        with exn ->
          LOG("can not get session bus address from property %s \
               on root window (maybe x11 is not running), error is: %s"
                session_bus_property (OBus_util.string_of_exn exn));
          return [default_session_bus_addresses]
)
