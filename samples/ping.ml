(*
 * ping.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Ping the pong service *)

open Lwt

let _ = Lwt_unix.run
  (perform
     bus <-- Lazy.force OBus_bus.session;

     (* Create a proxy for the remote object *)
     let proxy = OBus_proxy.make (OBus_peer.make bus "org.plop") ["plip"] in

     (* Send a ping *)
     let _ = print_endline "trying to ping the pong service..." in

     catch
       (fun _ -> perform
          msg <-- OBus_proxy.method_call proxy ~interface:"org.plop.foo" ~member:"ping" <:obus_func< string -> string >> "coucou";
          let _ = print_endline ("received: " ^ msg) in
          return ())
       (function
          | OBus_bus.Service_unknown msg ->
              print_endline "You must run pong to try this sample!";
              exit 1
          | exn -> fail exn))
