(*
 * nm_settings.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

open Lwt

include OBus_proxy.Private

open Nm_interfaces.Org_freedesktop_NetworkManagerSettings

let user () =
  let%lwt bus = OBus_bus.session () in
  return (OBus_proxy.make
            (OBus_peer.make bus "org.freedesktop.NetworkManagerUserSettings")
            [ "org"; "freedesktop"; "NetworkManagerSettings" ])

let system () =
  let%lwt bus = OBus_bus.system () in
  return (OBus_proxy.make
            (OBus_peer.make bus "org.freedesktop.NetworkManagerSystemSettings")
            [ "org"; "freedesktop"; "NetworkManagerSettings" ])

module Connection =
struct
  include OBus_proxy.Private

  open Nm_interfaces.Org_freedesktop_NetworkManagerSettings_Connection

  let update proxy ~properties =
    OBus_method.call m_Update proxy properties

  let delete proxy =
    OBus_method.call m_Delete proxy ()

  let get_settings proxy =
    OBus_method.call m_GetSettings proxy ()

  let updated proxy =
    OBus_signal.make s_Updated proxy

  let removed proxy =
    OBus_signal.make s_Removed proxy

  module Secrets =
  struct
    open Nm_interfaces.Org_freedesktop_NetworkManagerSettings_Connection_Secrets

    let get_secrets proxy ~setting_name ~hints ~request_new =
      OBus_method.call m_GetSecrets proxy (setting_name, hints, request_new)
  end
end

module System =
struct
  open Nm_interfaces.Org_freedesktop_NetworkManagerSettings_System

  let save_hostname proxy ~hostname =
    OBus_method.call m_SaveHostname proxy hostname

  let hostname proxy =
    OBus_property.make ~monitor:Nm_monitor.monitor p_Hostname proxy

  let can_modify proxy =
    OBus_property.make ~monitor:Nm_monitor.monitor p_CanModify proxy

  let properties_changed proxy =
    OBus_signal.make s_PropertiesChanged proxy

  let check_permissions proxy =
    OBus_signal.make s_CheckPermissions proxy

  let get_permissions proxy =
    let%lwt permissions = OBus_method.call m_GetPermissions proxy () in
    let permissions = Int32.to_int permissions in
    return permissions
end

let list_connections proxy =
  let%lwt (context, connections) = OBus_method.call_with_context m_ListConnections proxy () in
  return (
    List.map
      (fun path ->
         Connection.of_proxy
           (OBus_proxy.make (OBus_context.sender context) path))
      connections
  )

let add_connection proxy ~connection =
  OBus_method.call m_AddConnection proxy connection

let new_connection proxy =
  OBus_signal.map_with_context
    (fun context connection ->
       Connection.of_proxy (OBus_proxy.make (OBus_context.sender context) connection))
    (OBus_signal.make s_NewConnection proxy)
