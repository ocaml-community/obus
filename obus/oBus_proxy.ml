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

let kmethod_call cont ?interface ~member =
  OBus_connection.ksend_message_with_reply & fun f ->
    cont (fun proxy ->
            Lwt.bind
              (f (connection proxy)
                 (OBus_header.method_call
                    ?destination:(service proxy)
                    ~path:(path proxy)
                    ?interface
                    ~member ()))
              (fun (header, value) -> Lwt.return value))

let method_call ?interface ~member typ proxy = kmethod_call (fun f -> f proxy) ?interface ~member typ

let dmethod_call ?interface ~member proxy body =
  Lwt.bind
    (OBus_connection.dsend_message_with_reply (connection proxy)
       (OBus_header.method_call
          ?destination:(service proxy)
          ~path:(path proxy)
          ?interface
          ~member ())
       body)
    (fun (header, value) -> Lwt.return value)

let add_match bus =
  OBus_connection.method_call bus
    ~destination:"org.freedesktop.DBus"
    ~path:"/org/freedesktop/DBus"
    ~interface:"org.freedesktop.DBus"
    ~member:"AddMatch"
    (<< string -> unit >>)

let on_signal ~interface ~member typ proxy f =
  OBus_signal.add_receiver (connection proxy) ~interface ~member ~path:(path proxy) ?sender:(service proxy) typ (fun _ -> f)

let don_signal ~interface ~member proxy f =
  OBus_signal.dadd_receiver (connection proxy) ~interface ~member ~path:(path proxy) ?sender:(service proxy) (fun _ -> f)
