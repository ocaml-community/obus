(*
 * oBus_address.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(address)"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type guid = OBus_uuid.t

type t = { name : string; args : (string * string) list }

let name a = a.name

let args a = a.args

let make ~name ~args = { name; args }

let arg arg address = OBus_util.assoc arg address.args

let guid address =
  match OBus_util.assoc "guid" address.args with
  | Some guid -> Some (OBus_uuid.of_string guid)
  | None -> None

(* +-----------------------------------------------------------------+
   | Parsing/marshaling                                              |
   +-----------------------------------------------------------------+ *)

exception Parse_failure of string * int * string

let () =
  Printexc.register_printer (function
    | Parse_failure (str, pos, msg) ->
        Some
          (Printf.sprintf
             "failed to parse D-Bus addresses %S, at position %d: %s" str pos
             msg)
    | _ -> None)

let of_string str =
  try
    List.map
      (fun (name, args) -> { name; args })
      (OBus_address_lexer.addresses (Lexing.from_string str))
  with OBus_address_lexer.Fail (pos, msg) ->
    raise (Parse_failure (str, pos, msg))

let to_string l =
  let buf = Buffer.create 42 in
  let escape =
    String.iter (fun ch ->
        match ch with
        | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' | '/' | '.' | '\\' ->
            Buffer.add_char buf ch
        | _ -> Printf.bprintf buf "%%%02x" (Char.code ch))
  in
  let concat ch f = function
    | [] -> ()
    | x :: l ->
        f x;
        List.iter
          (fun x ->
            Buffer.add_char buf ch;
            f x)
          l
  in
  concat ';'
    (fun { name; args } ->
      Buffer.add_string buf name;
      Buffer.add_char buf ':';
      concat ','
        (fun (k, v) ->
          Buffer.add_string buf k;
          Buffer.add_char buf '=';
          escape v)
        args)
    l;
  Buffer.contents buf

(* +-----------------------------------------------------------------+
   | Well known addresses                                            |
   +-----------------------------------------------------------------+ *)

let system_bus_variable = "DBUS_SYSTEM_BUS_ADDRESS"

let session_bus_variable = "DBUS_SESSION_BUS_ADDRESS"

let xdg_runtime_dir_variable = "XDG_RUNTIME_DIR"

let default_system =
  [ { name = "unix"; args = [ ("path", "/var/run/dbus/system_bus_socket") ] } ]

let default_session = [ { name = "autolaunch"; args = [] } ]

let system =
  lazy
    ( match
        try Some (Sys.getenv system_bus_variable) with Not_found -> None
      with
    | Some str -> Lwt.return (of_string str)
    | None ->
        let%lwt () =
          Lwt_log.info_f ~section
            "environment variable %s not found, using internal default"
            system_bus_variable
        in
        Lwt.return default_system )

let xdg_fallback_session () =
  match
    try Some (Sys.getenv xdg_runtime_dir_variable) with Not_found -> None
  with
  | None -> Lwt.return_none
  | Some path ->
      Lwt.catch
        (fun () ->
          let sock_path = Filename.concat path "bus" in
          let%lwt stat = Lwt_unix.stat sock_path in
          let%lwt login = Lwt_unix.getlogin () in
          let%lwt user = Lwt_unix.getpwnam login in
          if stat.st_uid = user.pw_uid && stat.st_kind = Lwt_unix.S_SOCK then
            Lwt.return_some
              [ { name = "unix"; args = [ ("path", sock_path) ] } ]
          else Lwt.return_none)
        (fun _ -> Lwt.return_none)

let session =
  lazy
    ( match
        try Some (Sys.getenv session_bus_variable) with Not_found -> None
      with
    | Some line -> Lwt.return (of_string line)
    | None -> (
        let%lwt () =
          Lwt_log.info_f ~section
            "environment variable %s not found, trying XDG_RUNTIME_DIR/bus"
            session_bus_variable
        in
        let%lwt xdg_session = xdg_fallback_session () in
        match xdg_session with
        | Some session -> Lwt.return session
        | None -> (
            let%lwt () =
              Lwt_log.info_f ~section
                "failed to connect to %s/bus, trying to get session bus \
                 address from launchd"
                xdg_runtime_dir_variable
            in
            try%lwt
              let%lwt path =
                Lwt_process.pread_line
                  ( "launchctl",
                    [|
                      "launchctl"; "getenv"; "DBUS_LAUNCHD_SESSION_BUS_SOCKET";
                    |] )
              in
              Lwt.return [ { name = "unix"; args = [ ("path", path) ] } ]
            with exn ->
              let%lwt () =
                Lwt_log.info_f ~exn ~section
                  "failed to get session bus address from launchd, using \
                   internal default"
              in
              Lwt.return default_session ) ) )
