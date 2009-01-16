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

type match_rule = string
  with obus

type 'a t = {
  broadcast : bool;
  interface : OBus_name.interface;
  member : OBus_name.member;
  cast : OBus_connection.t * OBus_message.t -> 'a;
  get_proxy : unit -> OBus_proxy.t Lwt.t;
}

let make ?(broadcast=true) ~interface ~member ty get_proxy = {
  broadcast = broadcast;
  interface = interface;
  member = member;
  get_proxy = get_proxy;
  cast = fun (connection, message) -> OBus_type.cast_sequence ty
    ~context:(OBus_connection.Context(connection, message))
    (OBus_message.body message);
}

let dmake ?(broadcast=true) ~interface ~member get_proxy = {
  broadcast = broadcast;
  interface = interface;
  member = member;
  get_proxy = get_proxy;
  cast = fun (connection, message) -> OBus_message.body message;
}

let map f s = { s with cast = fun x -> f (s.cast x) }

type receiver = (unit -> unit) ref
    (* A receiver is just a disable function *)

let disconnect r =
  let f = !r in
  r := (fun _ -> ());
  f ()

(* Compute the arguments filter *)
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

let safe_cast cast x =
  try
    Some(cast x)
  with
    | OBus_type.Cast_failure ->
        None
    | exn ->
        Log.failure exn "message cast fail with";
        None

let connect signal ?(serial=false) ?(args=[]) func =
  signal.get_proxy () >>= fun proxy ->

    match (OBus_peer.connection (OBus_proxy.peer proxy))#get with
      | Crashed exn ->
          fail exn

      | Running connection ->
          let make_signal_receiver sender_opt = {
            sr_sender = sender_opt;
            sr_path = Some(OBus_proxy.path proxy);
            sr_interface = signal.interface;
            sr_member = signal.member;
            sr_args = make_args_filter args;
            sr_callback = make_callback serial
              (fun (connection, message) ->
                 match safe_cast signal.cast (connection, message) with
                   | Some x -> func x
                   | None -> return ());
          } in

          if connection.name = None then
            (* If the connection is a peer-to-peer connection the only
               thing to do is to locally add the receiver *)
            let node = MSet.add connection.signal_receivers (make_signal_receiver None) in
            return (ref (fun _ -> MSet.remove node))

          else begin

            let cont monitor resolver_opt signal_receiver =
              let match_rule = Match_rule.make
                ~typ:`signal
                ?sender:(OBus_peer.name (OBus_proxy.peer proxy))
                ~path:(OBus_proxy.path proxy)
                ~interface:signal.interface
                ~member:signal.member
                ~args ()
              and node = MSet.add connection.signal_receivers signal_receiver in
              let receiver = ref (fun _ ->
                                    MSet.remove node;
                                    begin match resolver_opt with
                                      | Some resolver ->
                                          OBus_resolver.disable resolver
                                      | None ->
                                          ()
                                    end;
                                    ignore (Bus.remove_match connection.packed match_rule)) in
              if monitor then
                (* If the peer has a unique name, then remove the receiver
                   when the peer exit *)
                ignore (OBus_peer.wait_for_exit (OBus_proxy.peer proxy) >>= fun _ -> disconnect receiver; return ());
              if signal.broadcast then
                Bus.add_match connection.packed match_rule >>= fun _ -> return receiver
              else
                return receiver
            in

            match OBus_peer.name (OBus_proxy.peer proxy) with
              | None ->
                  cont false None (make_signal_receiver None)

              | Some name ->
                  OBus_resolver.make connection.packed name >>= fun resolver ->
                    if OBus_name.is_unique name then begin
                      if not (OBus_resolver.owned resolver) then begin
                        OBus_resolver.disable resolver;
                        return (ref (fun _ -> ()))
                      end else
                        cont true (Some resolver) (make_signal_receiver (Some (OBus_resolver.internal_resolver resolver)))
                    end else
                      cont false (Some resolver) (make_signal_receiver (Some (OBus_resolver.internal_resolver resolver)))
          end
