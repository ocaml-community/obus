(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_internals

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

let kmethod_call cont =
  OBus_connection.ksend_message_with_reply & fun f ->
    cont (fun proxy ?interface ~member ->
            Lwt.bind
              (f (connection proxy)
                 (OBus_header.method_call
                    ?destination:(service proxy)
                    ~path:(path proxy)
                    ?interface
                    ~member ()))
              (fun (header, value) -> Lwt.return value))

let method_call proxy ?interface ~member = kmethod_call (fun f -> f proxy ?interface ~member)

let umethod_call proxy ?interface ~member body =
  Lwt.bind
    (OBus_connection.usend_message_with_reply (connection proxy)
       (OBus_header.method_call
          ?destination:(service proxy)
          ~path:(path proxy)
          ?interface
          ~member ())
       body)
    (fun (header, value) -> Lwt.return value)
