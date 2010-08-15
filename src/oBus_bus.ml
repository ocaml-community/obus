(*
 * oBus_bus.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(bus)"

open Lwt
open OBus_private_connection

(* +-----------------------------------------------------------------+
   | Message bus creation                                            |
   +-----------------------------------------------------------------+ *)

type t = OBus_connection.t

open OBus_interfaces.Org_freedesktop_DBus

let proxy bus =
  OBus_proxy.make (OBus_peer.make bus  "org.freedesktop.DBus") ["org"; "freedesktop"; "DBus"]

let error_handler = function
  | OBus_wire.Protocol_error msg ->
      ignore (Lwt_log.error_f ~section "the D-Bus connection with the message bus has been closed due to a protocol error: %s" msg);
      exit 1
  | OBus_connection.Connection_lost ->
      ignore (Lwt_log.info ~section "disconnected from D-Bus message bus");
      exit 0
  | OBus_connection.Transport_error exn ->
      ignore (Lwt_log.error_f ~section "the D-Bus connection with the message bus has been closed due to a transport error: %s" (Printexc.to_string exn));
      exit 1
  | exn ->
      ignore (Lwt_log.error ~section ~exn "the D-Bus connection with the message bus has been closed due to this uncaught exception");
      exit 1

let register_connection ?(set_on_disconnect=true) connection =
  let running = connection#get in
  match running.rc_name with
    | "" ->
        if set_on_disconnect then running.rc_on_disconnect := error_handler;
        lwt name = OBus_method.call m_Hello (proxy connection) () in
        running.rc_name <- name;
        return ()
    | _ ->
        (* Do not call two times the Hello method *)
        return ()

let of_addresses addresses =
  lwt bus = OBus_connection.of_addresses addresses ~shared:true in
  lwt () = register_connection bus in
  return bus

let session_bus = lazy(
  try_lwt
    Lazy.force OBus_address.session >>= of_addresses
  with exn ->
    lwt () = Lwt_log.warning ~exn ~section "Failed to open a connection to the session bus" in
    fail exn
)

let session () = Lazy.force session_bus

let system_bus_state = ref None
let system_bus_mutex = Lwt_mutex.create ()

let system () =
  Lwt_mutex.with_lock system_bus_mutex
    (fun () ->
       match !system_bus_state with
         | Some bus when React.S.value (OBus_connection.running bus) ->
             return bus
         | _ ->
             try_lwt
               lwt bus = Lazy.force OBus_address.system >>= of_addresses in
               system_bus_state := Some bus;
               return bus
             with exn ->
               lwt () = Lwt_log.warning ~exn ~section "Failed to open a connection to the system bus" in
               fail exn)

(* +-----------------------------------------------------------------+
   | Bindings to functions of the message bus                        |
   +-----------------------------------------------------------------+ *)

exception Access_denied of string
 with obus("org.freedesktop.DBus.Error.AccessDenied")

exception Service_unknown of string
 with obus("org.freedesktop.DBus.Error.ServiceUnknown")

exception Match_rule_not_found of string
 with obus("org.freedesktop.DBus.Error.MatchRuleNotFound")

exception Match_rule_invalid of string
 with obus("org.freedesktop.DBus.Error.MatchRuleInvalid")

exception Service_unknown of string
 with obus("org.freedesktop.DBus.Error.ServiceUnknown")

exception Name_has_no_owner of string
 with obus("org.freedesktop.DBus.Error.NameHasNoOwner")

exception Adt_audit_data_unknown of string
 with obus("org.freedesktop.DBus.Error.AdtAuditDataUnknown")

exception Selinux_security_context_unknown of string
 with obus("org.freedesktop.DBus.Error.SELinuxSecurityContextUnknown")

let acquired_names bus = bus#get.rc_acquired_names

type request_name_result = type_request_name_result

let request_name bus ?(allow_replacement=false) ?(replace_existing=false) ?(do_not_queue=false) name =
  let flags = [] in
  let flags = if allow_replacement then `Allow_replacement :: flags else flags in
  let flags = if replace_existing then `Replace_existing :: flags else flags in
  let flags = if do_not_queue then `Do_not_queue :: flags else flags in
  OBus_method.call m_RequestName (proxy bus) (name, cast_request_name_flags flags) >|= make_request_name_result

type release_name_result = type_release_name_result

let release_name bus name =
  OBus_method.call m_ReleaseName (proxy bus) name >|= make_release_name_result

type start_service_by_name_result = type_start_service_by_name_result

let start_service_by_name bus name =
  OBus_method.call m_StartServiceByName (proxy bus) (name, 0l) >|= make_start_service_by_name_result

let name_has_owner bus name =
  OBus_method.call m_NameHasOwner (proxy bus) name

let list_names bus =
  OBus_method.call m_ListNames (proxy bus) ()

let list_activatable_names bus =
  OBus_method.call m_ListActivatableNames (proxy bus) ()

let get_name_owner bus name =
  OBus_method.call m_GetNameOwner (proxy bus) name

let list_queued_owners bus name =
  OBus_method.call m_ListQueuedOwners (proxy bus) name

let add_match bus rule =
  OBus_method.call m_AddMatch (proxy bus) (OBus_match.string_of_rule rule)

let remove_match bus rule =
  OBus_method.call m_RemoveMatch (proxy bus) (OBus_match.string_of_rule rule)

let update_activation_environment bus data =
  OBus_method.call m_UpdateActivationEnvironment (proxy bus) data

let get_connection_unix_user bus name =
  OBus_method.call m_GetConnectionUnixUser (proxy bus) name >|= Int32.to_int

let get_connection_unix_process_id bus name =
  OBus_method.call m_GetConnectionUnixProcessID (proxy bus) name >|= Int32.to_int

let get_adt_audit_session_data bus name =
  OBus_method.call m_GetAdtAuditSessionData (proxy bus) name

let get_connection_selinux_security_context bus name =
  OBus_method.call m_GetConnectionSELinuxSecurityContext (proxy bus) name

let reload_config bus =
  OBus_method.call m_ReloadConfig (proxy bus) ()

let get_id bus =
  OBus_method.call m_GetId (proxy bus) () >|= OBus_uuid.of_string

let name_owner_changed bus =
  OBus_signal.connect s_NameOwnerChanged (proxy bus)

let name_lost bus =
  OBus_signal.connect s_NameLost (proxy bus)

let name_acquired bus =
  OBus_signal.connect s_NameAcquired (proxy bus)

let get_peer bus name =
  try_lwt
    lwt unique_name = get_name_owner bus name in
    return (OBus_peer.make bus unique_name)
  with Name_has_no_owner msg ->
    lwt _ = start_service_by_name bus name in
    lwt unique_name = get_name_owner bus name in
    return (OBus_peer.make bus unique_name)

let get_proxy bus name path =
  lwt peer = get_peer bus name in
  return (OBus_proxy.make peer path)
