(*
 * avahi.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type

module Address_resolver = struct
  include OBus_client.Make(struct let name = "org.freedesktop.Avahi.AddressResolver" end)
  let free = call "Free" << unit >>
  let on_found = on_signal "Found" << int -> int -> int -> string -> string -> uint -> unit >>
  let on_failure = on_signal "Failure" << string -> unit >>
end
module Domain_browser = struct
  include OBus_client.Make(struct let name = "org.freedesktop.Avahi.DomainBrowser" end)
  let free = call "Free" << unit >>
  let on_item_new = on_signal "ItemNew" << int -> int -> string -> uint -> unit >>
  let on_item_remove = on_signal "ItemRemove" << int -> int -> string -> uint -> unit >>
  let on_failure = on_signal "Failure" << string -> unit >>
  let on_all_for_now = on_signal "AllForNow" << unit -> unit >>
  let on_cache_exhausted = on_signal "CacheExhausted" << unit -> unit >>
end
module Entry_group = struct
  include OBus_client.Make(struct let name = "org.freedesktop.Avahi.EntryGroup" end)
  let free = call "Free" << unit >>
  let commit = call "Commit" << unit >>
  let reset = call "Reset" << unit >>
  let get_state = call "GetState" << int >>
  let on_state_changed = on_signal "StateChanged" << int -> string -> unit >>
  let is_empty = call "IsEmpty" << bool >>
  let add_service = call "AddService" << int -> int -> uint -> string -> string -> string -> string -> uint16 -> char list list -> unit >>
  let add_service_subtype = call "AddServiceSubtype" << int -> int -> uint -> string -> string -> string -> string -> unit >>
  let update_service_txt = call "UpdateServiceTxt" << int -> int -> uint -> string -> string -> string -> char list list -> unit >>
  let add_address = call "AddAddress" << int -> int -> uint -> string -> string -> unit >>
  let add_record = call "AddRecord" << int -> int -> uint -> string -> uint16 -> uint16 -> uint -> char list -> unit >>
end
module Host_name_resolver = struct
  include OBus_client.Make(struct let name = "org.freedesktop.Avahi.HostNameResolver" end)
  let free = call "Free" << unit >>
  let on_found = on_signal "Found" << int -> int -> string -> int -> string -> uint -> unit >>
  let on_failure = on_signal "Failure" << string -> unit >>
end
module Record_browser = struct
  include OBus_client.Make(struct let name = "org.freedesktop.Avahi.RecordBrowser" end)
  let free = call "Free" << unit >>
  let on_item_new = on_signal "ItemNew" << int -> int -> string -> uint16 -> uint16 -> char list -> uint -> unit >>
  let on_item_remove = on_signal "ItemRemove" << int -> int -> string -> uint16 -> uint16 -> char list -> uint -> unit >>
  let on_failure = on_signal "Failure" << string -> unit >>
  let on_all_for_now = on_signal "AllForNow" << unit -> unit >>
  let on_cache_exhausted = on_signal "CacheExhausted" << unit -> unit >>
end
module Service_browser = struct
  include OBus_client.Make(struct let name = "org.freedesktop.Avahi.ServiceBrowser" end)
  let free = call "Free" << unit >>
  let on_item_new = on_signal "ItemNew" << int -> int -> string -> string -> string -> uint -> unit >>
  let on_item_remove = on_signal "ItemRemove" << int -> int -> string -> string -> string -> uint -> unit >>
  let on_failure = on_signal "Failure" << string -> unit >>
  let on_all_for_now = on_signal "AllForNow" << unit -> unit >>
  let on_cache_exhausted = on_signal "CacheExhausted" << unit -> unit >>
end
module Service_resolver = struct
  include OBus_client.Make(struct let name = "org.freedesktop.Avahi.ServiceResolver" end)
  let free = call "Free" << unit >>
  let on_found = on_signal "Found" << int -> int -> string -> string -> string -> string -> int -> string -> uint16 -> char list list -> uint -> unit >>
  let on_failure = on_signal "Failure" << string -> unit >>
end
module Service_type_browser = struct
  include OBus_client.Make(struct let name = "org.freedesktop.Avahi.ServiceTypeBrowser" end)
  let free = call "Free" << unit >>
  let on_item_new = on_signal "ItemNew" << int -> int -> string -> string -> uint -> unit >>
  let on_item_remove = on_signal "ItemRemove" << int -> int -> string -> string -> uint -> unit >>
  let on_failure = on_signal "Failure" << string -> unit >>
  let on_all_for_now = on_signal "AllForNow" << unit -> unit >>
  let on_cache_exhausted = on_signal "CacheExhausted" << unit -> unit >>
end

module Server = struct
  include OBus_client.Make_constant(struct
                                      let name = "org.freedesktop.Avahi.Server"
                                      let path = OBus_path.empty
                                      let service = Some "org.freedesktop.Avahi"
                                      let bus = OBus_bus.system
                                    end)

  let get_version_string = call "GetVersionString" << unit -> string >>
  let get_apiversion = call "GetAPIVersion" << unit -> uint >>
  let get_host_name = call "GetHostName" << unit -> string >>
  let set_host_name = call "SetHostName" << string -> unit >>
  let get_host_name_fqdn = call "GetHostNameFqdn" << unit -> string >>
  let get_domain_name = call "GetDomainName" << unit -> string >>
  let is_nsssupport_available = call "IsNSSSupportAvailable" << unit -> bool >>
  let get_state = call "GetState" << unit -> int >>
  let on_state_changed = on_signal "StateChanged" << int -> string -> unit >>
  let get_local_service_cookie = call "GetLocalServiceCookie" << unit -> uint >>
  let get_alternative_host_name = call "GetAlternativeHostName" << string -> string >>
  let get_alternative_service_name = call "GetAlternativeServiceName" << string -> string >>
  let get_network_interface_name_by_index = call "GetNetworkInterfaceNameByIndex" << int -> string >>
  let get_network_interface_index_by_name = call "GetNetworkInterfaceIndexByName" << string -> int >>
  let resolve_host_name = call "ResolveHostName" << int -> int -> string -> int -> uint -> int * int * string * int * string * uint >>
  let resolve_address = call "ResolveAddress" << int -> int -> string -> uint -> int * int * int * string * string * uint >>
  let resolve_service = call "ResolveService" << int -> int -> string -> string -> string -> int -> uint -> int * int * string * string * string * string * int * string * uint16 * char list list * uint >>
  let entry_group_new = call "EntryGroupNew" << unit -> OBus_proxy.t >>
  let domain_browser_new = call "DomainBrowserNew" << int -> int -> string -> int -> uint -> OBus_proxy.t >>
  let service_type_browser_new = call "ServiceTypeBrowserNew" << int -> int -> string -> uint -> OBus_proxy.t >>
  let service_browser_new = call "ServiceBrowserNew" << int -> int -> string -> string -> uint -> OBus_proxy.t >>
  let service_resolver_new = call "ServiceResolverNew" << int -> int -> string -> string -> string -> int -> uint -> OBus_proxy.t >>
  let host_name_resolver_new = call "HostNameResolverNew" << int -> int -> string -> int -> uint -> OBus_proxy.t >>
  let address_resolver_new = call "AddressResolverNew" << int -> int -> string -> uint -> OBus_proxy.t >>
  let record_browser_new = call "RecordBrowserNew" << int -> int -> string -> uint16 -> uint16 -> uint -> OBus_proxy.t >>
end
