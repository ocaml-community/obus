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

let make ~connection ?destination ~path = {
  proxy_connection = connection;
  proxy_destination = destination;
  proxy_path = path;
}

let connection p = p.proxy_connection
let path p = p.proxy_path
let destination p = p.proxy_destination

let compare a b = Pervasives.compare (destination a, path a) (destination b, path b)

let kmethod_call cont ?interface ~member typ =
  call_and_cast_reply typ & fun body f ->
    cont & fun proxy ->
      f (connection proxy)
        (OBus_message.method_call
           ?destination:(destination proxy)
           ~path:(path proxy)
           ?interface
           ~member body)

let method_call ?interface ~member typ proxy =
  method_call (connection proxy)
    ?destination:(destination proxy)
    ~path:(path proxy)
    ?interface
    ~member
    typ

let dmethod_call ?interface ~member proxy body =
  dmethod_call (connection proxy)
    ?destination:(destination proxy)
    ~path:(path proxy)
    ?interface
    ~member
    body

let on_signal ?global ~interface ~member typ proxy f =
  OBus_signal.add_receiver (connection proxy) ?global ~interface ~member
    ~path:(path proxy) ?sender:(destination proxy) typ f

let don_signal ?global ~interface ~member proxy f =
  OBus_signal.dadd_receiver (connection proxy) ?global ~interface ~member
    ~path:(path proxy) ?sender:(destination proxy) f

let property ~interface ~name ~access typ proxy =
  OBus_property.make ~interface ~name ~access ~connection:(connection proxy) ?destination:(destination proxy) ~path:(path proxy) typ

let dproperty ~interface ~name ~access proxy =
  OBus_property.dmake ~interface ~name ~access ~connection:(connection proxy) ?destination:(destination proxy) ~path:(path proxy)
