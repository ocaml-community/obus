(*
 * oBus_signal.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_private

class type ['a] t = object
  method event : 'a React.event
  method disconnect : unit
end

let _connect proxy ~interface ~member ~push ~until =
  match (OBus_proxy.connection proxy)#get with
    | Crashed exn ->
        fail exn

    | Running connection ->
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
          lwt () = until in
          Lwt_sequence.remove node;
          return ()

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
                lwt () = until in
                Lwt_sequence.remove node;
                OBus_private_bus.remove_match connection.packed match_rule

            | Some name ->
                lwt resolver = OBus_resolver.make connection.packed name in
                let node = Lwt_sequence.add_r (make_signal_receiver (Some resolver#name)) connection.signal_receivers in
                lwt () = OBus_private_bus.add_match connection.packed match_rule in
                lwt () = until in
                Lwt_sequence.remove node;
                OBus_private_bus.remove_match connection.packed match_rule <&> resolver#disable
        end

let dyn_connect proxy ~interface ~member =
  let event, push = React.E.create () and until_waiter, until_wakener = wait () in
  ignore_result (_connect proxy ~interface ~member ~push ~until:until_waiter);
  let event = React.E.map (fun (connection, message) -> OBus_message.body message) event
  and disconnect = lazy(wakeup until_wakener ()) in
  (object
     method event = event
     method disconnect = Lazy.force disconnect
   end)

let cast interface member typ (connection, message) =
  try
    Some(OBus_type.cast_sequence typ ~context:(OBus_connection.make_context (connection, message)) (OBus_message.body message))
  with exn ->
    LogI#exn exn "failed to cast signal from %S, interface %S, member %S with signature %S to %S"
      (match OBus_message.sender message with None -> "" | Some n -> n) interface member
      (OBus_value.string_of_signature (OBus_value.type_of_sequence (OBus_message.body message)))
      (OBus_value.string_of_signature (OBus_type.type_sequence typ));
    None

let connect proxy ~interface ~member typ =
  let event, push = React.E.create () and until_waiter, until_wakener = wait () in
  ignore_result (_connect proxy ~interface ~member ~push ~until:until_waiter);
  let event = React.E.fmap (cast interface member typ) event
  and disconnect = lazy(wakeup until_wakener ()) in
  (object
     method event = event
     method disconnect = Lazy.force disconnect
   end)
