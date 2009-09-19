(*
 * oBus_property.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

type 'a access = 'a
let rd_only = `readable
let wr_only = `writable
let rdwr = `readable

type proxy =
  | Simple of OBus_proxy.t
  | Custom of (unit -> OBus_proxy.t Lwt.t)

type ('a, 'access) t = {
  interface : OBus_name.interface;
  member : OBus_name.member;
  cast : OBus_type.context -> OBus_value.single -> 'a;
  make : 'a -> OBus_value.single;
  proxy : proxy;
}

let _make interface member access typ proxy = {
  interface = interface;
  member = member;
  proxy = proxy;
  cast = (fun context v -> OBus_type.cast_single ~context typ v);
  make = (fun v -> OBus_type.make_single typ v);
}

let _dyn_make interface member access proxy = {
  interface = interface;
  member = member;
  proxy = proxy;
  cast = (fun _ v -> v);
  make = (fun v -> v);
}

let make ~interface ~member ~access typ proxy = _make interface member access typ (Simple proxy)
let dyn_make ~interface ~member ~access proxy = _dyn_make interface member access (Simple proxy)
let make_custom ~interface ~member ~access typ f = _make interface member access typ (Custom f)
let dyn_make_custom ~interface ~member ~access f = _dyn_make interface member access (Custom f)

let with_proxy p f = match p with
  | Simple proxy -> f proxy
  | Custom get -> get () >>= f

let method_call member typ proxy = OBus_proxy.method_call proxy ~interface:"org.freedesktop.DBus.Properties" ~member typ

let obus_context = OBus_type.wrap_with_context obus_unit (fun context _ -> context) (fun _ -> ())

OBUS_method Get : string -> string -> variant * context
OBUS_method Set : string -> string -> variant -> unit
OBUS_method GetAll : string -> (string, variant) dict

let get prop =
  with_proxy prop.proxy
    (fun proxy -> get proxy prop.interface prop.member >>= fun (v, ctx) -> return (prop.cast ctx v))

let set prop x =
  with_proxy prop.proxy
    (fun proxy -> set proxy prop.interface prop.member (prop.make x))
