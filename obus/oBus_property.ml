(*
 * oBus_property.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type
open OBus_type.Pervasives
open OBus_value

type access = [ `readable | `writable ]
let rd_only = `readable
let wr_only = `writable
let rdwr = `readable

type ('a, 'access) t = {
  interface : OBus_name.interface;
  member : OBus_name.member;
  ty : 'a OBus_type.ty_single;
  get_proxy : unit -> OBus_proxy.t Lwt.t;
}

let make  ~interface ~member ~access typ get_proxy = {
  interface = interface;
  member = member;
  get_proxy = get_proxy;
  ty = OBus_type.wrap_single_ctx tvariant
    (fun context v -> OBus_type.cast_single ~context typ v)
    (fun v -> OBus_type.make_single typ v)
}

let dmake  ~interface ~member ~access get_proxy = {
  interface = interface;
  member = member;
  ty = tvariant;
  get_proxy = get_proxy;
}

let interface = "org.freedesktop.DBus.Properties"

let get prop = prop.get_proxy () >>= fun proxy -> OBus_proxy.call proxy ~interface ~member:"Get" (tstring --> (tstring --> reply prop.ty)) prop.interface prop.member
let set prop x = prop.get_proxy () >>= fun proxy -> OBus_proxy.call proxy ~interface ~member:"Set" (tstring --> (tstring --> (prop.ty --> reply tunit))) prop.interface prop.member x
let get_all proxy iface = OBus_proxy.call proxy ~interface ~member:"GetAll" << string -> {string, variant} list >> iface
