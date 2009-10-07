(*
 * errors.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_connection

let filter message = assert false
let handler x = failwith "plop"

let _ =
  OBus_info.verbose := true;
  OBus_info.debug := true;
  Printexc.record_backtrace true;
  ignore (Lwt_sequence.add_r filter (incoming_filters loopback));
  Lwt_event.always_notify handler (OBus_signal.dyn_connect (OBus_proxy.make (OBus_peer.anonymous loopback) []) "aa" "plop")#event;
  Lwt_main.run begin
    lwt () = dyn_emit_signal loopback ~interface:"aa.aa" ~member:"plop" ~path:[] [] in
    lwt () = Lwt_unix.sleep 0.5 in
    lwt () = close loopback in
    dyn_emit_signal loopback ~interface:"aa.aa" ~member:"plop" ~path:[] []
  end

