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

OBUS_method Hello : name

let error_handler = function
  | OBus_lowlevel.Protocol_error msg ->
      Log.error "the DBus connection with the message bus has been closed due to a protocol error: %s" msg;
      exit 1
  | OBus_connection.Connection_lost ->
      Log.log "disconnected from DBus message bus";
      exit 0
  | OBus_lowlevel.Transport_error exn ->
      Log.error "the DBus connection with the message bus has been closed due to a transport error: %s" (Util.string_of_exn exn);
      exit 1
  | exn ->
      Log.error "the DBus connection with the message bus has been closed due to this uncaught exception: %s" (Printexc.to_string exn);
      exit 1

let register_connection connection =
  lwt_with_running connection
    (function
       | { name = Some _ } ->
           (* Do not call two times the Hello method *)
           return ()
       | { name = None; on_disconnect = f } ->
           f := error_handler;
           hello connection >>= fun name ->
             lwt_with_running connection
               (fun running ->
                  running.name <- Some name;
                  return ()))

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

OBUS_method RequestName : string -> request_name_flag_list -> request_name_result

OBUS_flag release_name_result : uint =
    [ 1 -> `released
    | 2 -> `non_existent
    | 3 -> `not_owner ]

OBUS_method ReleaseName : string -> release_name_result

type start_service_flag
let tstart_service_flag : start_service_flag list ty_basic = wrap_basic tuint (fun _ -> []) (fun _ -> 0)

OBUS_flag start_service_by_name_result : uint =
  [ 1 -> `success
  | 2 -> `already_running ]

OBUS_method StartServiceByName : string -> start_service_flag -> start_service_by_name_result
OBUS_method NameHasOwner : string -> bool
OBUS_method ListNames : name list
OBUS_method ListActivatableNames : name list
OBUS_method GetNameOwner : name -> name
OBUS_method ListQueuedOwners : name -> name list

OBUS_type match_rule = string

let match_rule = Rules.to_string

OBUS_method AddMatch : match_rule -> unit
OBUS_method RemoveMatch : match_rule -> unit

OBUS_method GetConnectionUnixUser : string -> int
OBUS_method GetConnectionUnixProcessId : string -> int
OBUS_method GetConnectionSelinuxSecurityContext : string -> byte_array
OBUS_method ReloadConfig : unit
OBUS_method GetId : OBus_uuid.t

OBUS_signal NameOwnerChanged : name * name * name
OBUS_signal NameLost : name
OBUS_signal NameAcquired : name

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
  <:obus_type< string * string * string >>
  (fun (_, o, n) -> f (status o, status n))

let on_client_exit bus name f =
  let w = wait () in
  let called = ref false in
  ignore_result
    (perform
       id <-- OBus_signal.dadd_receiver bus
         ~sender:"org.freedesktop.DBus"
         ~path:["org"; "freedesktop"; "DBus"]
         ~interface:"org.freedesktop.DBus"
         ~member:"NameOwnerChanged"
         ~args:[(0, name); (1, name); (2, "")]
         (fun _ -> match !called with
            | false -> wakeup w (); f ()
            | true -> ());
       w;
       OBus_signal.disable_receiver id)
