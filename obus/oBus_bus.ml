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

include OBus_interface.Make_custom
  (struct
     type t = OBus_connection.t
     let make_proxy connection =
       return { OBus_proxy.peer = { OBus_peer.connection = connection;
                                    OBus_peer.name = Some "org.freedesktop.DBus" };
                OBus_proxy.path = ["org"; "freedesktop"; "DBus"] }
   end)
  (struct
     let name = "org.freedesktop.DBus"
   end)

OBUS_method Hello : string

let error_handler = function
  | OBus_lowlevel.Protocol_error msg ->
      Log.error "the DBus connection with the message bus has been closed due to a protocol error: %s" msg;
      exit 1
  | OBus_connection.Connection_lost ->
      Log.log "disconnected from DBus message bus";
      exit 0
  | OBus_connection.Transport_error exn ->
      Log.error "the DBus connection with the message bus has been closed due to a transport error: %s" (Util.string_of_exn exn);
      exit 1
  | exn ->
      Log.failure exn "the DBus connection with the message bus has been closed due to this uncaught exception";
      exit 1

let register_connection connection = match connection#get with
  | Crashed exn ->
      fail exn

  | Running connection -> match connection.name with
      | Some _ ->
          (* Do not call two times the Hello method *)
          return ()

      | None ->
          connection.on_disconnect := error_handler;
          hello connection.packed >>= fun name ->
            connection.name <- Some name;
            return ()

let of_addresses addresses =
  OBus_connection.of_addresses addresses ~shared:true >>= fun bus ->
    perform
      register_connection bus;
      return bus

let of_laddresses laddr = Lazy.force laddr >>= of_addresses

let session = lazy(of_laddresses OBus_address.session)
let system = lazy(of_laddresses OBus_address.system)

OBUS_exception Error.ServiceUnknown
OBUS_exception Error.MatchRuleNotFound
OBUS_exception Error.ServiceUnknown
OBUS_exception Error.NameHasNoOwner

let acquired_names bus = match bus#get with
  | Crashed exn -> raise exn
  | Running connection -> connection.acquired_names

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
let request_name bus ?(flags=[]) name = request_name bus name flags

OBUS_flag release_name_result : uint =
    [ 1 -> `released
    | 2 -> `non_existent
    | 3 -> `not_owner ]

OBUS_method ReleaseName : string -> release_name_result

OBUS_flag start_service_by_name_result : uint =
  [ 1 -> `success
  | 2 -> `already_running ]

OBUS_method StartServiceByName : string -> uint -> start_service_by_name_result
let start_service_by_name bus name = start_service_by_name bus name 0
OBUS_method NameHasOwner : string -> bool
OBUS_method ListNames : string list
OBUS_method ListActivatableNames : string list
OBUS_method GetNameOwner : string -> string
OBUS_method ListQueuedOwners : string -> string list

OBUS_type match_rule = Match_rule.t

let match_rule = Match_rule.make

OBUS_method AddMatch : match_rule -> unit
OBUS_method RemoveMatch : match_rule -> unit

OBUS_method GetConnectionUnixUser : string -> int
OBUS_method GetConnectionUnixProcessId : string -> int
OBUS_method GetConnectionSelinuxSecurityContext : string -> byte_array
OBUS_method ReloadConfig : unit
OBUS_method GetId : OBus_uuid.t

let tname_opt = OBus_type.wrap_basic tstring
  (function
     | "" -> None
     | str -> Some str)
  (function
     | None -> ""
     | Some str -> str)

OBUS_signal NameOwnerChanged : string * name_opt * name_opt
OBUS_signal NameLost : string
OBUS_signal NameAcquired : string

let get_peer bus name =
  try_bind
    (fun _ -> get_name_owner bus name)
    (fun n -> return (OBus_peer.make bus n))
    (function
       | Name_has_no_owner _ ->
           (perform
              start_service_by_name bus name;
              n <-- get_name_owner bus name;
              return (OBus_peer.make bus n))
       | exn -> fail exn)

let get_proxy bus name path =
  (perform
     peer <-- get_peer bus name;
     return (OBus_proxy.make peer path))
