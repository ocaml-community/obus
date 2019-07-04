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

open Ping_pong.Org_foo_bar

let ping proxy msg =
  OBus_method.call m_Ping proxy msg

let _ = Lwt_main.run begin
  let%lwt bus = OBus_bus.session () in

  (* Create a proxy for the remote object *)
  let proxy = OBus_proxy.make (OBus_peer.make bus "org.plop") ["plip"] in

  (* Send a ping *)
  let%lwt () = printl "trying to ping the pong service..." in

  try%lwt
    let%lwt msg = ping proxy "coucou" in
    printlf "received: %s" msg
  with
    | OBus_bus.Name_has_no_owner msg ->
        let%lwt () = printl "You must run pong to try this sample!" in
        exit 1
    | exn ->
        Lwt.fail exn
end
