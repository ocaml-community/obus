(*
 * oBus_peer.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

type t = {
  connection : OBus_connection.t;
  name : OBus_name.bus option;
}

let compare = Pervasives.compare

let connection p = p.connection
let name p = p.name

let make ~connection ~name = { connection = connection; name = Some name }
let anonymous c = { connection = c; name = None }

let ping peer =
  lwt context, () =
    OBus_private_method.call_with_context
      ~connection:peer.connection
      ?destination:peer.name
      ~path:[]
      ~interface:"org.freedesktop.DBus.Peer"
      ~member:"Ping"
      ~i_args:OBus_value.C.seq0
      ~o_args:OBus_value.C.seq0
      ()
  in
  return { connection = peer.connection;
           name = context.OBus_private_connection.mc_sender }

let get_machine_id peer =
  OBus_private_method.call
    ~connection:peer.connection
    ?destination:peer.name
    ~path:[]
    ~interface:"org.freedesktop.DBus.Peer"
    ~member:"GetMachineId"
    ~i_args:OBus_value.C.seq0
    ~o_args:(OBus_value.C.seq1 OBus_value.C.basic_string)
    ()
  >|= OBus_uuid.of_string

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

type peer = t

module type Private = sig
  type t = private peer
  external of_peer : peer -> t = "%identity"
  external to_peer : t -> peer = "%identity"
end

module Private =
struct
  type t = peer
  external of_peer : peer -> t = "%identity"
  external to_peer : t -> peer = "%identity"
end
