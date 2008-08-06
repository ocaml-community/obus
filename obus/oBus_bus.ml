(*
 * oBus_bus.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_intern

include OBus_client.Make_fixed_path
  (struct
     let name = "org.freedesktop.DBus"
     let path = "/org/freedesktop/DBus"
     let service = Some "org.freedesktop.DBus"
   end)

type name = string

let ob_name = ob_string

let hello bus = call bus "Hello" [: name ]

let name bus = with_running bus $ fun running -> running.name
let register_connection connection =
  hello connection >>= fun name ->
    lwt_with_running connection $ fun running ->
      running.name <- name;
      return ()

let of_addresses addresses =
  (perform
     connection <-- OBus_connection.of_addresses addresses ~shared:true;
     register_connection connection;
     return connection)

let session = lazy(of_addresses (Lazy.force OBus_address.session))
let system = lazy(of_addresses (Lazy.force OBus_address.system))

OBUS_EXN Name_has_no_owner = "Error.NameHasNoOwner"
OBUS_EXN Match_rule_not_found = "Error.MatchRuleNotFound"

OBUS_BITWISE request_name_flag [:uint] =
  [ 1 -> `allow_replacement
  | 2 -> `replace_existing
  | 4 -> `do_not_queue ]

OBUS_FLAG request_name_result [:uint] =
  [ 1 -> `primary_owner
  | 2 -> `in_queue
  | 3 -> `exists
  | 4 -> `already_owner ]

let request_name bus = call bus "RequestName" [: string -> request_name_flag_list -> request_name_result ]

OBUS_FLAG release_name_result [:uint] =
    [ 1 -> `released
    | 2 -> `non_existent
    | 3 -> `not_owner ]

let release_name bus = call bus "ReleaseName" [: string -> release_name_result ]

type start_service_flag
let ob_start_service_flag =
  OBus_comb.make OBus_types.duint32
    (OBus_wire.failwith "not implemented")
    (fun _ -> OBus_wire.wuint 0)

OBUS_FLAG start_service_by_name_result [:uint] =
  [ 1 -> `success
  | 2 -> `already_running ]

let start_service_by_name bus = call bus "StartServiceByName" [: string -> start_service_flag -> start_service_by_name_result ]
let name_has_owner bus = call bus "NameHasOwner" [: string -> bool ]
let list_names bus = call bus "ListNames" [: name list ]
let list_activable_names bus = call bus "ListActivatableNames" [: name list ]
let get_name_owner bus = call bus "GetNameOwner" [: name -> name ]
let list_queued_owners bus = call bus "ListQueuedOwners" [: name -> name list ]

type match_rule = string
let ob_match_rule = ob_string

let match_rule ?typ ?sender ?interface ?member ?path ?destination ?(args=[]) () =
  let buf = Buffer.create 42 in
  let first = ref true in
  let coma () =
    if !first
    then first := false
    else Buffer.add_char buf ',' in
  let add key value =
    coma ();
    Printf.bprintf buf "%s='%s'" key value in
  let add_opt key = function
    | None -> ()
    | Some x -> add key x in
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
    add_opt "sender" sender;
    add_opt "interface" interface;
    add_opt "member" member;
    add_opt "path" path;
    add_opt "destination" destination;
    List.iter (fun (n, value) -> coma (); Printf.bprintf buf "arg%d='%s'" n value) args;
    Buffer.contents buf

let add_match bus = call bus "AddMatch" [: match_rule -> unit ]
let remove_match bus = call bus "RemoveMatch" [: match_rule -> unit ]
let get_connection_unix_user bus = call bus "GetConnectionUnixUser" [: string -> int ]
let get_connection_unix_process_id bus = call bus "GetConnectionUnixProcessId" [: string -> int ]
let get_connection_selinux_security_context bus = call bus "GetConnectionSelinuxSecurityContext" [: string -> byte_array ]
let reload_config bus = call bus "ReloadConfig" [: unit ]
let get_id bus = call bus "GetId" [: string ]

(*let name_owner_changed = signal "NameOwnerChanged" [: string -> string -> string -> unit ]
let name_lost = signal "NameLost" [: string -> unit ]
let name_acquired = signal "NameAcquired" [: string -> unit ]*)
