(*
 * avahi.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type

module Address_resolver = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.AddressResolver" end)
  OBUS_method Free: unit
  OBUS_signal! Found : int * int * int * string * string * uint
  OBUS_signal! Failure : string
end
module Domain_browser = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.DomainBrowser" end)
  OBUS_method Free: unit
  OBUS_signal! ItemNew : int * int * string * uint
  OBUS_signal! ItemRemove : int * int * string * uint
  OBUS_signal! Failure : string
  OBUS_signal! AllForNow : unit
  OBUS_signal! CacheExhausted : unit
end
module Entry_group = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.EntryGroup" end)
  OBUS_method Free: unit
  OBUS_method Commit: unit
  OBUS_method Reset: unit
  OBUS_method GetState: int
  OBUS_signal! StateChanged : int * string
  OBUS_method IsEmpty: bool
  OBUS_method AddService: int -> int -> uint -> string -> string -> string -> string -> uint16 -> char list list -> unit
  OBUS_method AddServiceSubtype: int -> int -> uint -> string -> string -> string -> string -> unit
  OBUS_method UpdateServiceTxt: int -> int -> uint -> string -> string -> string -> char list list -> unit
  OBUS_method AddAddress: int -> int -> uint -> string -> string -> unit
  OBUS_method AddRecord: int -> int -> uint -> string -> uint16 -> uint16 -> uint -> char list -> unit
end
module Host_name_resolver = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.HostNameResolver" end)
  OBUS_method Free: unit
  OBUS_signal! Found : int * int * string * int * string * uint
  OBUS_signal! Failure : string
end
module Record_browser = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.RecordBrowser" end)
  OBUS_method Free: unit
  OBUS_signal! ItemNew : int * int * string * uint16 * uint16 * char list * uint
  OBUS_signal! ItemRemove : int * int * string * uint16 * uint16 * char list * uint
  OBUS_signal! Failure : string
  OBUS_signal! AllForNow : unit
  OBUS_signal! CacheExhausted : unit
end
module Service_browser = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.ServiceBrowser" end)
  OBUS_method Free: unit
  OBUS_signal! ItemNew : int * int * string * string * string * uint
  OBUS_signal! ItemRemove : int * int * string * string * string * uint
  OBUS_signal! Failure : string
  OBUS_signal! AllForNow : unit
  OBUS_signal! CacheExhausted : unit
end
module Service_resolver = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.ServiceResolver" end)
  OBUS_method Free: unit
  OBUS_signal! Found : int * int * string * string * string * string * int * string * uint16 * char list list * uint
  OBUS_signal! Failure : string
end
module Service_type_browser = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.ServiceTypeBrowser" end)
  OBUS_method Free: unit
  OBUS_signal! ItemNew : int * int * string * string * uint
  OBUS_signal! ItemRemove : int * int * string * string * uint
  OBUS_signal! Failure : string
  OBUS_signal! AllForNow : unit
  OBUS_signal! CacheExhausted : unit
end

module Server = struct
  include OBus_interface.Make(struct let name = "org.freedesktop.Avahi.Server" end)

  OBUS_method GetVersionString : string
  OBUS_method GetAPIVersion : uint
  OBUS_method GetHostName : string
  OBUS_method SetHostName : string -> unit
  OBUS_method GetHostNameFqdn : string
  OBUS_method GetDomainName : string
  OBUS_method IsNSSSupportAvailable : bool
  OBUS_method GetState : int
  OBUS_signal StateChanged  : int * string
  OBUS_method GetLocalServiceCookie : uint
  OBUS_method GetAlternativeHostName : string -> string
  OBUS_method GetAlternativeServiceName : string -> string
  OBUS_method GetNetworkInterfaceNameByIndex : int -> string
  OBUS_method GetNetworkInterfaceIndexByName : string -> int
  OBUS_method ResolveHostName : int -> int -> string -> int -> uint -> int * int * string * int * string * uint
  OBUS_method ResolveAddress : int -> int -> string -> uint -> int * int * int * string * string * uint
  OBUS_method ResolveService : int -> int -> string -> string -> string -> int -> uint -> int * int * string * string * string * string * int * string * uint16 * char list list * uint
  OBUS_method EntryGroupNew : OBus_proxy.t
  OBUS_method DomainBrowserNew : int -> int -> string -> int -> uint -> OBus_proxy.t
  OBUS_method ServiceTypeBrowserNew : int -> int -> string -> uint -> OBus_proxy.t
  OBUS_method ServiceBrowserNew : int -> int -> string -> string -> uint -> OBus_proxy.t
  OBUS_method ServiceResolverNew : int -> int -> string -> string -> string -> int -> uint -> OBus_proxy.t
  OBUS_method HostNameResolverNew : int -> int -> string -> int -> uint -> OBus_proxy.t
  OBUS_method AddressResolverNew : int -> int -> string -> uint -> OBus_proxy.t
  OBUS_method RecordBrowserNew : int -> int -> string -> uint16 -> uint16 -> uint -> OBus_proxy.t
end

let path = []
let name = "org.freedesktop.Avahi"

let peer = lazy(perform
                  bus <-- Lazy.force OBus_bus.system;
                  return (OBus_peer.make bus name))

let server = lazy(perform
                    peer <-- Lazy.force peer;
                    return (OBus_proxy.make peer path))
