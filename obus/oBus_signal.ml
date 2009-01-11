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
  cast : OBus_connection.t * OBus_message.signal -> 'a;
}

let make ?(broadcast=true) ~interface ~member ty = {
  broadcast = broadcast;
  interface = interface;
  member = member;
  cast = fun (connection, message) -> cast_sequence ty
    ~context:(Context(connection, (message :> OBus_message.any)))
    (OBus_message.body message);
}

let dmake ?(broadcast=true) ~interface ~member = {
  broadcast = broadcast;
  interface = interface;
  member = member;
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
    | Cast_failure ->
        None
    | exn ->
        Log.failure exn "message cast fail with";
        None

let connect_backend callback_func peer path_mask signal ?(serial=false) ?(args=[]) func =
  let connection = peer.connection in

  let make_signal_receiver sender_opt = {
    sr_sender = sender_opt;
    sr_path = path_mask;
    sr_interface = signal.interface;
    sr_member = signal.member;
    sr_args = make_args_filter args;
    sr_callback = make_callback serial callback_func;
  } in

  if not connection#is_bus then
    (* If the connection is a peer-to-peer connection the only thing
       to do is to locally add the receiver *)
    let node = connection#add_signal_receiver (make_signal_receiver None) in
    return (ref (fun _ -> MSet.remove node))

  else begin

    let cont monitor resolver_opt signal_receiver =
      let match_rule = Match_rule.make
        ~typ:`signal
        ?sender:peer.name
        ?path:path_mask
        ~interface:signal.interface
        ~member:signal.member
        ~args ()
      and node = connection#add_signal_receiver signal_receiver in
      let receiver = ref (fun _ ->
                            MSet.remove node;
                            begin match resolver_opt with
                              | Some resolver ->
                                  OBus_resolver.disable resolver
                              | None ->
                                  ()
                            end;
                            ignore (Bus.remove_match connection match_rule)) in
      if monitor then
        (* If the peer has a unique name, then remove the receiver
           when the peer exit *)
        ignore (OBus_peer.wait_for_exit peer >>= fun _ -> disconnect receiver; return ());
      if signal.broadcast then
        Bus.add_match connection match_rule >>= fun _ -> return receiver
      else
        return receiver
    in

    match peer.name with
      | None ->
          cont false None (make_signal_receiver None)

      | Some name ->
          OBus_resolver.make connection name >>= fun resolver ->
            if OBus_name.is_unique name then begin
              if not (OBus_resolver.owned resolver) then begin
                OBus_resolver.disable resolver;
                return (ref (fun _ -> ()))
              end else
                cont true (Some resolver) (make_signal_receiver (Some (OBus_resolver.internal_resolver resolver)))
            end else
              cont false (Some resolver) (make_signal_receiver (Some (OBus_resolver.internal_resolver resolver)))
  end

let connect proxy signal ?serial ?args func =
  connect_backend
    (fun (connection, message) ->
       match safe_cast signal.cast (connection, message) with
         | Some x -> func x
         | None -> return ())
    proxy.peer (Some proxy.path) signal ?serial ?args func

let connect_any peer signal ?serial ?args func =
  connect_backend
    (fun (connection, message) ->
       match safe_cast signal.cast (connection, message) with
         | Some x ->
             let `Signal(path, _, _) = message.OBus_message.typ in
             func { OBus_proxy.peer = { OBus_peer.connection = connection;
                                        OBus_peer.name = message.OBus_message.sender };
                    OBus_proxy.path = path } x
         | None -> return ())
    peer None signal ?serial ?args func
