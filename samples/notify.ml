(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

let main =
  (perform
     Notification.notify ~summary:"Hello, world!" ~body:"ocaml is fun!" ~icon:"info" ();
     Lwt_unix.sleep 0.5;

     id <-- Notification.notify ~summary:"Actions test" ~body:"click on something!"
       ~category:"network"
       ~actions:[("coucou", `coucou);
                 ("plop", `plop)] ();

     result <-- Notification.result id;

     let _ = match result with
       | `coucou -> print_endline "You pressed coucou!"
       | `plop -> print_endline "You pressed plop!"
       | `default -> print_endline "default action invoked"
       | `closed -> print_endline "notification closed"
     in

     return ())

let _ = Lwt_unix.run main
