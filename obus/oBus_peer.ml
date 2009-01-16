(*
 * oBus_peer.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

type t = {
  connection : OBus_connection.t;
  name : OBus_name.bus option;
} with projection

let obus_t = OBus_type.wrap_with_context <:obus_type< unit >>
  (fun context () -> match context with
     | OBus_connection.Context(connection, msg) -> { connection = connection; name = OBus_message.sender msg }
     | _ -> raise OBus_type.Cast_failure)
  (fun _ -> ())

let make c n = { connection = c; name = Some n }
let anonymous c = { connection = c; name = None }

let call member typ peer = OBus_connection.method_call peer.connection
  ?destination:peer.name
  ~path:[]
  ~interface:"org.freedesktop.DBus.Peer"
  ~member
  typ

OBUS_method Ping : t
OBUS_method GetMachineId : OBus_uuid.t

let wait_for_exit peer =
  match peer.name with
    | Some name ->
        let exited = ref false and w = Lwt.wait () in
        (perform
           resolver <-- OBus_resolver.make
             ~on_change:(fun owner -> match owner, !exited with
                           | None, false ->
                               exited := true;
                               Lwt.wakeup w ();
                               return ()
                           | _ ->
                               return ())
             peer.connection name;
           w;
           let _ = OBus_resolver.disable resolver in
           return ())

    | None ->
        fail (Invalid_argument "OBus_peer.wait_for_exit: peer has no name")
