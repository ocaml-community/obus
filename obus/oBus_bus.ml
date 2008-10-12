(*
 * oBus_bus.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_internals
open OBus_type

include OBus_client.Make_constant_path
  (struct
     let name = "org.freedesktop.DBus"
     let path = ["org"; "freedesktop"; "DBus"]
     let service = Some "org.freedesktop.DBus"
   end)

OBUS_type name = string

let hello = call "Hello" << name >>

let register_connection connection =
  lwt_with_running connection & function
    | { name = Some _ } ->
        (* Do not call two times the Hello method *)
        return ()
    | { name = None } ->
        hello connection >>= fun name ->
          lwt_with_running connection & fun running ->
            running.name <- Some name;
            return ()

let of_addresses addresses =
  (perform
     connection <-- OBus_connection.of_addresses addresses ~shared:true;
     register_connection connection;
     return connection)

let session = lazy(of_addresses (Lazy.force OBus_address.session))
let system = lazy(of_addresses (Lazy.force OBus_address.system))

OBUS_exn Service_unknown = "Error.ServiceUnknown"
OBUS_exn Name_has_no_owner = "Error.NameHasNoOwner"
OBUS_exn Match_rule_not_found = "Error.MatchRuleNotFound"
OBUS_exn Service_unknown = "Error.ServiceUnknown"

OBUS_bitwise request_name_flag : uint =
  [ 1 -> `allow_replacement
  | 2 -> `replace_existing
  | 4 -> `do_not_queue ]

OBUS_flag request_name_result : uint =
  [ 1 -> `primary_owner
  | 2 -> `in_queue
  | 3 -> `exists
  | 4 -> `already_owner ]

let request_name = call "RequestName" << string -> request_name_flag_list -> request_name_result >>

OBUS_flag release_name_result : uint =
    [ 1 -> `released
    | 2 -> `non_existent
    | 3 -> `not_owner ]

let release_name = call "ReleaseName" << string -> release_name_result >>

type start_service_flag
let tstart_service_flag : start_service_flag list ty_basic = wrap_basic tuint (fun _ -> []) (fun _ -> 0)

OBUS_flag start_service_by_name_result : uint =
  [ 1 -> `success
  | 2 -> `already_running ]

let start_service_by_name = call "StartServiceByName" << string -> start_service_flag -> start_service_by_name_result >>
let name_has_owner = call "NameHasOwner" << string -> bool >>
let list_names = call "ListNames" << name list >>
let list_activable_names = call "ListActivatableNames" << name list >>
let get_name_owner = call "GetNameOwner" << name -> name >>
let list_queued_owners = call "ListQueuedOwners" << name -> name list >>

OBUS_type match_rule = string

let match_rule = Rules.to_string

let add_match = call "AddMatch" << match_rule -> unit >>
let remove_match = call "RemoveMatch" << match_rule -> unit >>

let get_connection_unix_user = call "GetConnectionUnixUser" << string -> int >>
let get_connection_unix_process_id = call "GetConnectionUnixProcessId" << string -> int >>
let get_connection_selinux_security_context = call "GetConnectionSelinuxSecurityContext" << string -> byte_array >>
let reload_config = call "ReloadConfig" << unit >>
let get_id = call "GetId" << uuid >>

let on_name_owner_changed = on_signal "NameOwnerChanged" << string -> string -> string -> unit >>
let on_name_lost = on_signal "NameLost" << string -> unit >>
let on_name_acquired = on_signal "NameAcquired" << string -> unit >>

type status = [ `up | `down ]

let status = function
  | "" -> `down
  | _ -> `up

let on_service_status_change bus service f = OBus_signal.add_receiver bus
  ~sender:"org.freedesktop.DBus"
  ~path:["org"; "freedesktop"; "DBus"]
  ~interface:"org.freedesktop.DBus"
  ~member:"NameOwnerChanged"
  ~args:[0, service]
  << string -> string -> string -> unit >>
  (fun _ o n -> f (status o, status n))
