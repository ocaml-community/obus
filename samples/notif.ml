(*
 * notif.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open Notify

let main =
  (perform
     notify ~summary:"Hello, world!" ~body:"ocaml is fun!" ~icon:"info" ();
     Lwt_unix.sleep 0.5;
     notify ~summary:"Test 2" ~category:"network" ~actions:[("a", "coucou"); ("b", "hoho")] ();
     return ())

let _ = Lwt_unix.run main
