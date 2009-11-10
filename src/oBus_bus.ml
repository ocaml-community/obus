(*
 * oBus_bus.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_private
open OBus_type.Perv

let op_interface = "org.freedesktop.DBus"

let make bus =
  { OBus_proxy.peer = { OBus_peer.connection = bus;
                        OBus_peer.name = Some op_interface };
    OBus_proxy.path = ["org"; "freedesktop"; "DBus"] }

let op_method_call member typ bus = OBus_proxy.method_call (make bus) ~interface:op_interface ~member typ
let op_signal member typ bus = OBus_signal.connect (make bus) ~interface:op_interface ~member typ
let op_property_reader member typ bus = OBus_property.get (make bus) ~interface:op_interface ~member typ
let op_property_writer member typ bus value = OBus_property.set (make bus) ~interface:op_interface ~member typ value

type t = OBus_connection.t

OP_method Hello : string

let error_handler = function
  | OBus_wire.Protocol_error msg ->
      Log#error "the D-Bus connection with the message bus has been closed due to a protocol error: %s" msg;
      exit 1
  | OBus_connection.Connection_lost ->
      Log#info "disconnected from D-Bus message bus";
      exit 0
  | OBus_connection.Transport_error exn ->
      Log#error "the D-Bus connection with the message bus has been closed due to a transport error: %s" (OBus_util.string_of_exn exn);
      exit 1
  | exn ->
      Log#exn exn "the D-Bus connection with the message bus has been closed due to this uncaught exception";
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
          lwt name = hello connection.packed in
          connection.name <- Some name;
          return ()

let of_addresses addresses =
  lwt bus = OBus_connection.of_addresses addresses ~shared:true in
  lwt () = register_connection bus in
  return bus

let of_laddresses laddr = Lazy.force laddr >>= of_addresses

let session = lazy(of_laddresses OBus_address.session)
let system = lazy(of_laddresses OBus_address.system)

let prefix = op_interface ^ ".Error."

exception Service_unknown of string
 with obus(prefix ^ "ServiceUnknown")

exception OBus_match_not_found of string
 with obus(prefix ^ "MatchRuleNotFound")

exception Service_unknown of string
 with obus(prefix ^ "ServiceUnknown")

exception Name_has_no_owner of string
 with obus(prefix ^ "NameHasNoOwner")

let acquired_names bus = match bus#get with
  | Crashed exn -> raise exn
  | Running connection -> connection.acquired_names

type request_name_result =
    [ `primary_owner
    | `in_queue
    | `exists
    | `already_owner ]

let obus_request_name_result = OBus_type.mapping obus_uint
  [`primary_owner, 1; `in_queue, 2; `exists, 3; `already_owner, 4]

OP_method RequestName : string -> uint -> request_name_result
let request_name bus ?(allow_replacement=false) ?(replace_existing=false) ?(do_not_queue=false) name =
  request_name bus name ((if allow_replacement then 1 else 0) lor
                           (if replace_existing then 2 else 0) lor
                           (if do_not_queue then 4 else 0))

type release_name_result = [ `released | `non_existent | `not_owner ]

let obus_release_name_result = OBus_type.mapping obus_uint
  [`released, 1; `non_existent, 2; `not_owner, 3]

OP_method ReleaseName : string -> release_name_result

type start_service_by_name_result = [ `success | `already_running ]

let obus_start_service_by_name_result = OBus_type.mapping obus_uint
  [(`success, 1); (`already_running, 2)]

OP_method StartServiceByName : string -> uint -> start_service_by_name_result
let start_service_by_name bus name = start_service_by_name bus name 0
OP_method NameHasOwner : string -> bool
OP_method ListNames : string list
OP_method ListActivatableNames : string list
OP_method GetNameOwner : string -> string
OP_method ListQueuedOwners : string -> string list

OP_method AddMatch : OBus_match.rule -> unit
OP_method RemoveMatch : OBus_match.rule -> unit

OP_method GetConnectionUnixUser : string -> int
OP_method GetConnectionUnixProcessId : string -> int
OP_method GetConnectionSelinuxSecurityContext : string -> byte_array
OP_method ReloadConfig : unit
OP_method GetId : OBus_uuid.t

let obus_name_opt = OBus_type.map obus_string
  (function
     | "" -> None
     | str -> Some str)
  (function
     | None -> ""
     | Some str -> str)

OP_signal NameOwnerChanged : string * name_opt * name_opt
OP_signal NameLost : string
OP_signal NameAcquired : string

let get_peer bus name =
  try_lwt
    lwt unique_name = get_name_owner bus name in
    return (OBus_peer.make bus unique_name)
  with Name_has_no_owner _ ->
    lwt _ = start_service_by_name bus name in
    lwt unique_name = get_name_owner bus name in
    return (OBus_peer.make bus unique_name)

let get_proxy bus name path =
  lwt peer = get_peer bus name in
  return (OBus_proxy.make peer path)
