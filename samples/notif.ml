(*
 * notif.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

let main =
  (perform
     Notify.notify ~summary:"Hello, world!" ~body:"ocaml is fun!" ~icon:"info" ();
     Lwt_unix.sleep 0.5;

     id <-- Notify.notify ~summary:"Actions test" ~body:"click on something!"
       ~category:"network"
       ~actions:[("coucou", `Coucou);
                 ("plop", `Plop)] ();

     result <-- Notify.result id;

     let _ = match result with
       | `Coucou -> print_endline "You pressed coucou!"
       | `Plop -> print_endline "You pressed plop!"
       | `Closed -> print_endline "notification closed"
     in

     return ())

let _ = Lwt_unix.run main
