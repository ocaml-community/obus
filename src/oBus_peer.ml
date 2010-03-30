(*
 * oBus_peer.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_pervasives

type t = {
  connection : OBus_connection.t;
  name : OBus_name.bus option;
} with projection

let obus_t = OBus_type.map_with_context <:obus_type< unit >>
  (fun context () ->
     let connection, message = OBus_connection.cast_context context in
     { connection = connection; name = OBus_message.sender message })
  ignore

let make ~connection ~name = { connection = connection; name = Some name }
let anonymous c = { connection = c; name = None }

let ping peer =
  OBus_connection.method_call peer.connection
    ?destination:peer.name
    ~path:[]
    ~interface:"org.freedesktop.DBus.Peer"
    ~member:"Ping"
    <:obus_func< t >>

let get_machine_id peer =
  OBus_connection.method_call peer.connection
    ?destination:peer.name
    ~path:[]
    ~interface:"org.freedesktop.DBus.Peer"
    ~member:"GetMachineId"
    <:obus_func< OBus_uuid.t >>

let wait_for_exit peer =
  match peer.name with
    | Some name ->
        let waiter, wakener = Lwt.wait () in
        lwt resolver = OBus_resolver.make peer.connection name in
        let ev = React.S.map (function
                                | None ->
                                    if Lwt.state waiter = Sleep then Lwt.wakeup wakener ()
                                | Some _ ->
                                    ()) (OBus_resolver.owner resolver) in
        lwt () = waiter in
        (* Just to make the compiler happy: *)
        ignore ev;
        OBus_resolver.disable resolver

    | None ->
        fail (Invalid_argument "OBus_peer.wait_for_exit: peer has no name")

(* +-----------------------------------------------------------------+
   | Private peers                                                   |
   +-----------------------------------------------------------------+ *)

type peer = t with obus

module type Private = sig
  type t = private peer
  val obus_t : t OBus_type.sequence
  external of_peer : peer -> t = "%identity"
  external to_peer : t -> peer = "%identity"
end

module Private =
struct
  type t = peer with obus
  external of_peer : peer -> t = "%identity"
  external to_peer : t -> peer = "%identity"
end
