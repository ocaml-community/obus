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
  IFDEF HAVE_BACKTRACE THEN
    Printexc.record_backtrace true;
  END;
  ignore (add_incoming_filter loopback filter);
  ignore (OBus_signal.connect (OBus_proxy.make (OBus_peer.anonymous loopback) [])
            (OBus_signal.dmake "aa" "plop")
            handler);
  Lwt_unix.run
    (perform
       demit_signal loopback ~interface:"aa.aa" ~member:"plop" ~path:[] [];
       Lwt_unix.sleep 0.5;
       let _ = close loopback in
       demit_signal loopback ~interface:"aa.aa" ~member:"plop" ~path:[] [])

