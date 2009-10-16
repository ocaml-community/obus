(*
 * ping.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Ping the pong service *)

open Lwt
open Lwt_io
open OBus_type.Perv

include OBus_interface.Make(struct let name = "org.plop.foo" end)

OP_method Ping : string -> string

let _ = Lwt_main.run begin
  lwt bus = Lazy.force OBus_bus.session in

  (* Create a proxy for the remote object *)
  let proxy = OBus_proxy.make (OBus_peer.make bus "org.plop") ["plip"] in

  (* Send a ping *)
  lwt () = printl "trying to ping the pong service..." in

  try_lwt
    lwt msg = ping proxy "coucou" in
    printlf "received: %s" msg
  with
    | OBus_bus.Service_unknown msg ->
        lwt () = printl "You must run pong to try this sample!" in
        exit 1
    | exn ->
        fail exn
end
