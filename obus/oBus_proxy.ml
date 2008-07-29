(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)


type t = {
  connection : OBus_connection.t;
  service : string option;
  path : OBus_path.t;
}

let make ~connection ?service ~path = {
  connection = connection;
  service = service;
  path = path;
}

let connection p = p.connection
let path p = p.path
let service p = p.service

let kmethod_call cont proxy ?interface ~member =
  OBus_connection.ksend_message_with_reply (fun w -> Lwt.bind w (fun (header, value) -> cont value))
    proxy.connection
    (OBus_header.method_call
       ?destination:proxy.service
       ~path:proxy.path
       ?interface
       ~member ())

let method_call p = kmethod_call (fun x -> Lwt.return x) p

let umethod_call proxy ?interface ~member body =
  Lwt.bind
    (OBus_connection.usend_message_with_reply proxy.connection
       (OBus_header.method_call
          ?destination:proxy.service
          ~path:proxy.path
          ?interface
          ~member ())
       body)
    (fun (header, value) -> Lwt.return value)

open OBus_wire

let ob_t = OBus_comb.make
  ~annot:OBus_annot.dobject_path
  ~reader:(perform
             path <-- robject_path;
             connection <-- rconnection;
             sender <-- rsender;
             return (make ~connection ?service:sender ~path))
  ~writer:(fun proxy -> wobject_path proxy.path)
