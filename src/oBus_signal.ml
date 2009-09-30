(*
 * oBus_signal.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_private

class type ['a] t = object
  method event : 'a React.event
  method disconnect : unit Lwt.t
end

let _connect proxy ~interface ~member =
  match (OBus_proxy.connection proxy)#get with
    | Crashed exn ->
        fail exn

    | Running connection ->
        let event, push = React.E.create () in

        let make_signal_receiver sender_resolver_opt = {
          sr_sender = sender_resolver_opt;
          sr_path = OBus_proxy.path proxy;
          sr_interface = interface;
          sr_member = member;
          sr_push = push;
        } in

        if connection.name = None then
          (* If the connection is a peer-to-peer connection the only
             thing to do is to locally add the receiver *)
          let node = Lwt_sequence.add_r (make_signal_receiver None) connection.signal_receivers in
          let disable = lazy(Lwt_sequence.remove node; return ()) in
          return (event, disable)

        else begin

          let match_rule = OBus_match.rule
            ~typ:`signal
            ?sender:(OBus_proxy.name proxy)
            ~path:(OBus_proxy.path proxy)
            ~interface
            ~member
            ()
          in

          match OBus_proxy.name proxy with
            | None ->
                let node = Lwt_sequence.add_r (make_signal_receiver None) connection.signal_receivers in
                lwt () = OBus_private_bus.add_match connection.packed match_rule in
                let disable = lazy(Lwt_sequence.remove node;
                                   OBus_private_bus.remove_match connection.packed match_rule) in
                return (event, disable)

            | Some name ->
                lwt resolver = OBus_resolver.make connection.packed name in
                let node = Lwt_sequence.add_r (make_signal_receiver (Some resolver#name)) connection.signal_receivers in
                lwt () = OBus_private_bus.add_match connection.packed match_rule in
                let disable = lazy(Lwt_sequence.remove node;
                                   OBus_private_bus.remove_match connection.packed match_rule <&> resolver#disable) in
                return (event, disable)
        end

let dyn_connect proxy ~interface ~member =
  lwt event, disconnect = _connect proxy ~interface ~member in
  let event = React.E.map OBus_message.body event in
  return (object
            method event = event
            method disconnect = Lazy.force disconnect
          end)

let connect proxy ~interface ~member typ =
  lwt event, disconnect = _connect proxy ~interface ~member in
  let event = React.E.fmap begin fun message ->
    try
      Some(OBus_type.cast_sequence typ ~context:(OBus_connection.Context(OBus_proxy.connection proxy, message)) (OBus_message.body message))
    with exn ->
      FAILURE(exn, "failed to cast signal from %S, interface %S, member %S with signature %S to %S"
                (match OBus_message.sender message with None -> "" | Some n -> n) interface member
                (OBus_value.string_of_signature (OBus_value.type_of_sequence (OBus_message.body message)))
                (OBus_value.string_of_signature (OBus_type.type_sequence typ)));
      None
  end event in
  return (object
            method event = event
            method disconnect = Lazy.force disconnect
          end)
