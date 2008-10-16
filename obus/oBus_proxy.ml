(*
 * oBus_proxy.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type
open OBus_connection
open Lwt

type t = {
  connection : OBus_connection.t;
  destination : OBus_name.connection option;
  path : OBus_path.t;
}

let make ~connection ?destination ~path = {
  connection = connection;
  destination = destination;
  path = path;
}

let connection p = p.connection
let path p = p.path
let destination p = p.destination

let tt = OBus_type.wrap_basic_ctx OBus_type.tobject_path
  (fun context path -> match context with
     | Context(connection, msg) ->
         { connection = connection;
           destination = OBus_message.sender msg;
           path = path }
     | _ -> raise Cast_failure)
  path

let kmethod_call cont ?interface ~member typ =
  call_and_cast_reply typ begin fun body f ->
    cont begin fun proxy ->
      f (connection proxy)
        (OBus_message.method_call
           ?destination:(destination proxy)
           ~path:(path proxy)
           ?interface
           ~member body)
    end
  end

let method_call proxy ?interface ~member typ =
  method_call (connection proxy)
    ?destination:(destination proxy)
    ~path:(path proxy)
    ?interface
    ~member
    typ

let dmethod_call proxy ?interface ~member body =
  dmethod_call (connection proxy)
    ?destination:(destination proxy)
    ~path:(path proxy)
    ?interface
    ~member
    body

let on_signal proxy ?global ~interface ~member typ f =
  OBus_signal.add_receiver (connection proxy) ?global ~interface ~member
    ~path:(path proxy) ?sender:(destination proxy) typ f

let don_signal proxy ?global ~interface ~member f =
  OBus_signal.dadd_receiver (connection proxy) ?global ~interface ~member
    ~path:(path proxy) ?sender:(destination proxy) f

let property proxy ~interface ~member ~access typ =
  OBus_property.make ~interface ~member ~access ~connection:(connection proxy) ?destination:(destination proxy) ~path:(path proxy) typ

let dproperty proxy ~interface ~member ~access =
  OBus_property.dmake ~interface ~member ~access ~connection:(connection proxy) ?destination:(destination proxy) ~path:(path proxy)

let set proxy ~interface ~member typ x =
  OBus_property.set (property proxy ~interface ~member ~access:`writable typ) x

let get proxy ~interface ~member typ =
  OBus_property.get (property proxy ~interface ~member ~access:`readable typ)

let dset proxy ~interface ~member x =
  OBus_property.dset (dproperty proxy ~interface ~member ~access:`writable) x

let dget proxy ~interface ~member =
  OBus_property.dget (dproperty proxy ~interface ~member ~access:`readable)

let dget_all proxy ~interface =
  OBus_property.dget_all ~interface ~connection:(connection proxy) ?destination:(destination proxy) ~path:(path proxy)
