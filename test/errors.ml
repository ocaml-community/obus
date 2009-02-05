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
  ignore (add_incoming_filter loopback filter);
  ignore (OBus_signal.connect
            (OBus_signal.dyn_make "aa" "plop" (OBus_proxy.make (OBus_peer.anonymous loopback) []))
            handler);
  Lwt_unix.run
    (perform
       dyn_emit_signal loopback ~interface:"aa.aa" ~member:"plop" ~path:[] [];
       Lwt_unix.sleep 0.5;
       let _ = close loopback in
       dyn_emit_signal loopback ~interface:"aa.aa" ~member:"plop" ~path:[] [])

