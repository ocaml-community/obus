(*
 * oBus_signal.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_internals
open OBus_connection
open OBus_type
open OBus_peer
open OBus_proxy

OBUS_type match_rule = string

type 'a t = {
  broadcast : bool;
  interface : OBus_name.interface;
  member : OBus_name.member;
  cast : OBus_connection.t -> OBus_message.signal -> 'a;
}

let make ?(broadcast=true) ~interface ~member ty = {
  broadcast = broadcast;
  interface = interface;
  member = member;
  cast = fun connection message -> cast_sequence ty
    ~context:(Context(connection, (message :> OBus_message.any)))
    (OBus_message.body message);
}

let dmake ?(broadcast=true) ~interface ~member = {
  broadcast = broadcast;
  interface = interface;
  member = member;
  cast = fun connection message -> OBus_message.body message;
}

let map f s = { s with cast = fun connection message -> f (s.cast connection message) }

type receiver = match_rule option * OBus_connection.t * signal_receiver MSet.node

(* Compute the argument filters *)
let make_args_filter l =
  let rec aux prev = function
    | [] -> []
    | (n, p) :: l ->
        if n = prev then
          aux n l
        else
          (n - prev - 1, p) :: aux n l
  in
  aux (-1) (List.sort (fun (x, _) (y, _) -> x - y) l)

let call member connection = function
  | None -> return ()
  | Some m ->
      if is_bus connection then
        method_call connection
          ~destination:"org.freedesktop.DBus"
          ~path:["org"; "freedesktop"; "DBus"]
          ~interface:"org.freedesktop.DBus"
          ~member
          (<< match_rule -> unit >>)
          m
      else
        return ()

let enable (mr, connection, node) =
  match MSet.is_alone node with
    | false -> return ()
    | true ->
        lwt_with_running
          (fun connection ->
             MSet.insert node connection.signal_receivers;
             call "AddMatch" connection mr)
          connection

let disable (mr, connection, node) =
  match MSet.is_alone node with
    | true -> return ()
    | false ->
        lwt_with_running
          (fun connection ->
             MSet.remove node;
             call "RemoveMatch" connection mr)
          connection

let enabled (mr, connection, node) = not (MSet.is_alone node)

let connect proxy signal ?(args=[]) func =
  let id = ((match signal.broadcast with
               | true -> Some(Rules.to_string ~typ:`signal
                                ?sender:proxy.peer.name
                                ~path:proxy.path
                                ~interface:signal.interface
                                ~member:signal.member
                                ~args ())
               | false -> None),
            proxy.peer.connection,
            MSet.node { sr_sender = proxy.peer.name;
                        sr_path = Some proxy.path;
                        sr_interface = signal.interface;
                        sr_member = signal.member;
                        sr_args = make_args_filter args;
                        sr_handler = fun connection message ->
                          try
                            func (signal.cast connection message)
                          with
                            | Cast_failure -> ()
                            | exn -> Log.failure exn "signal handler fail with" }) in
  enable id >>= fun _ -> return id

let connect_any peer signal ?(args=[]) func =
  let id = ((match signal.broadcast with
               | true -> Some(Rules.to_string ~typ:`signal
                                ?sender:peer.name
                                ~interface:signal.interface
                                ~member:signal.member
                                ~args ())
               | false -> None),
            peer.connection,
            MSet.node { sr_sender = peer.name;
                        sr_path = None;
                        sr_interface = signal.interface;
                        sr_member = signal.member;
                        sr_args = make_args_filter args;
                        sr_handler = fun connection message ->
                          try
                            func { peer = { connection = connection; name = OBus_message.sender message };
                                   path = OBus_message.path message }
                              (signal.cast connection message)
                          with
                            | Cast_failure -> ()
                            | exn -> Log.failure exn "signal handler fail with" }) in
  enable id >>= fun _ -> return id
