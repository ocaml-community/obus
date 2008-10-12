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

let compare a b = Pervasives.compare (destination a, path a) (destination b, path b)

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
