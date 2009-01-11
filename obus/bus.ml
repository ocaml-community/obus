(*
 * bus.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let destination = "org.freedesktop.DBus"
let path = ["org"; "freedesktop"; "DBus"]
let interface = "org.freedesktop.DBus"

let add_match connection =
  OBus_connection.method_call_no_reply connection ~member:"AddMatch" ~destination ~path ~interface (<< Match_rule.t -> unit >>)

let remove_match connection =
  OBus_connection.method_call_no_reply connection ~member:"RemoveMatch" ~destination ~path ~interface (<< Match_rule.t -> unit >>)

let get_name_owner connection name =
  Lwt.try_bind
    (fun _ -> OBus_connection.method_call connection ~member:"GetNameOwner" ~destination ~path ~interface (<< string -> string >>) name)
    (fun n -> Lwt.return (Some n))
    (fun _ -> Lwt.return None)
