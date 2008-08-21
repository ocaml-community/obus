(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_internals
open OBus_type
open OBus_connection
open Lwt

type t = proxy

let make ~connection ?service ~path = {
  proxy_connection = connection;
  proxy_service = service;
  proxy_path = path;
}

let connection p = p.proxy_connection
let path p = p.proxy_path
let service p = p.proxy_service

let compare a b = Pervasives.compare (service a, path a) (service b, path b)

let kmethod_call cont ?interface ~member typ =
  call_and_cast_reply typ & fun body f ->
    cont & fun proxy ->
      f (connection proxy)
        (OBus_message.method_call
           ?destination:(service proxy)
           ~path:(path proxy)
           ?interface
           ~member body)

let method_call ?interface ~member typ proxy =
  method_call (connection proxy)
    ?destination:(service proxy)
    ~path:(path proxy)
    ?interface
    ~member
    typ

let dmethod_call ?interface ~member proxy body =
  dmethod_call (connection proxy)
    ?destination:(service proxy)
    ~path:(path proxy)
    ?interface
    ~member
    body

let on_signal ?no_match_rule ~interface ~member typ proxy f =
  OBus_signal.add_receiver ?no_match_rule (connection proxy) ~interface ~member
    ~path:(path proxy) ?sender:(service proxy) typ f

let don_signal ?no_match_rule ~interface ~member proxy f =
  OBus_signal.dadd_receiver ?no_match_rule  (connection proxy) ~interface ~member
    ~path:(path proxy) ?sender:(service proxy) f

let property ~interface ~name ~access typ proxy =
  OBus_property.make ~interface ~name ~access ~connection:(connection proxy) ?service:(service proxy) ~path:(path proxy) typ

let dproperty ~interface ~name ~access proxy =
  OBus_property.dmake ~interface ~name ~access ~connection:(connection proxy) ?service:(service proxy) ~path:(path proxy)
