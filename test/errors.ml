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
  ignore (add_filter loopback filter);
  ignore (dadd_signal_receiver loopback handler);
  Lwt_unix.run
    (perform
       demit_signal loopback ~interface:"aa.aa" ~member:"plop" ~path:[] [];
       Lwt_unix.sleep 0.5;
       let _ = (transport loopback)#abort (Failure "plip") in
       demit_signal loopback ~interface:"aa.aa" ~member:"plop" ~path:[] [])

