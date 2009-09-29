(*
 * oBus_peer.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type.Perv

type t = {
  connection : OBus_connection.t;
  name : OBus_name.bus option;
} with projection

let obus_t = OBus_type.map_with_context <:obus_type< unit >>
  (fun context () -> match context with
     | OBus_connection.Context(connection, message) ->
         { connection = connection; name = OBus_message.sender message }
     | _ ->
         raise OBus_type.Cast_failure)
  (fun _ -> ())

let make c n = { connection = c; name = Some n }
let anonymous c = { connection = c; name = None }

let method_call member typ peer = OBus_connection.method_call peer.connection
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
        let waiter, wakener = Lwt.wait () in
        lwt resolver = OBus_resolver.make peer.connection name in
        let ev = React.S.map (function
                                | None ->
                                    if Lwt.state waiter = Sleep then Lwt.wakeup wakener ()
                                | Some _ ->
                                    ()) resolver#name in
        lwt () = waiter in
        (* Just to make the compiler happy: *)
        ignore ev;
        resolver#disable

    | None ->
        fail (Invalid_argument "OBus_peer.wait_for_exit: peer has no name")
