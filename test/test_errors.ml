(*
 * test_errors.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open OBus_connection

let connection = OBus_connection.loopback ()

let filter message = assert false
let handler x = failwith "plop"

let _ =
  Printexc.record_backtrace true;
  ignore (Lwt_sequence.add_r filter (incoming_filters connection));
  Lwt_event.always_notify handler (OBus_signal.dyn_connect (OBus_proxy.make (OBus_peer.anonymous connection) []) "aa" "plop")#event;
  Lwt_main.run begin
    lwt () = dyn_emit_signal connection ~interface:"aa.aa" ~member:"plop" ~path:[] [] in
    lwt () = Lwt_unix.sleep 0.5 in
    lwt () = close connection in
    dyn_emit_signal connection ~interface:"aa.aa" ~member:"plop" ~path:[] []
  end

