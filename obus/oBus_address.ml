(*
 * oBus_address.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module Log = Log.Make(struct let section = "address" end)

type name = string
type key = string
type value = string
type guid = OBus_uuid.t

type tcp_params = {
  tcp_host : string;
  tcp_bind : string;
  tcp_port : string;
  tcp_family : [ `Ipv4 | `Ipv6 ] option;
}

type desc =
  | Unix_path of string
  | Unix_abstract of string
  | Unix_tmpdir of string
  | Tcp of tcp_params
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
                     Log.error "invalid unix address: must specify exactly one of \"path\", \"abstract\" or \"tmpdir\"";
                     Unknown(name, params)
             end
           | "tcp" ->
               Tcp { tcp_host = assoc "host" "" params;
                     tcp_port = assoc "port" "0" params;
                     tcp_bind = assoc "bind" "*" params;
                     tcp_family = match Util.assoc "family" params with
                       | Some "ipv4" -> Some `Ipv4
                       | Some "ipv6" -> Some `Ipv4
                       | Some f ->
                           Log.error "unknown address family: %S" f;
                           None
                       | None -> None }
           | "autolaunch" -> Autolaunch
           | _ -> Unknown(name, params)
         end,
         match Util.assoc "guid" params with
           | Some(guid_hex_encoded) -> Some(OBus_uuid.of_string guid_hex_encoded)
           | None -> None) end addresses
  with
      Failure msg ->
        Log.debug "failed to parse address %S: %s" str msg;
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
  concat ':' begin fun (desc, guid) ->
    let name, params = match desc with
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
          "tcp", (Util.filter_map (fun x -> x)
                    [(if host <> "" then Some("host", host) else None);
                     (if bind <> "" && bind <> "*" then Some("bind", bind) else None);
                     (if port <> "0" then Some("port", port) else None);
                     (Util.wrap_option family
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

let tlist = OBus_type.wrap_basic OBus_type.tstring of_string to_string

let system_bus_variable = "DBUS_SYSTEM_BUS_ADDRESS"
let session_bus_variable = "DBUS_SESSION_BUS_ADDRESS"
let session_bus_property = "_DBUS_SESSION_BUS_ADDRESS"

let default_session_bus_addresses = [Autolaunch, None]
let default_system_bus_addresses = [Unix_path "/var/run/dbus/system_bus_socket", None]

open Lwt

let system = lazy begin
  match
    try Some (Sys.getenv system_bus_variable) with
        Not_found ->
          Log.debug "environment variable %s not found, using internal default" system_bus_variable;
          None
  with
    | Some str -> Util.exn_to_lwt of_string str
    | None -> return default_system_bus_addresses
end

let session = lazy begin
  catch
    (fun _ -> perform
       line <-- catch (fun _ -> return (Sys.getenv session_bus_variable))
         (fun _ ->
            Log.log "environment variable %s not found" session_bus_variable;
            catch
              (fun _ ->
                 (* Try with the root window property, this is bit ugly
                    and it depends on the presence of xprop... *)
                 Util.with_process_in "xprop" [|"xprop"; "-root"; session_bus_property|]
                   (fun ic -> perform
                      line <-- Lwt_chan.input_line ic;
                      Scanf.sscanf line "_DBUS_SESSION_BUS_ADDRESS(STRING) = %S" return))
              (fun exn ->
                 Log.log "can not get session bus address from property %s \
                          on root window (maybe x11 is not running), error is: %s"
                   session_bus_property (Util.string_of_exn exn);
                 fail exn));
       return (of_string line))
    (fun _ -> return default_session_bus_addresses)
end
