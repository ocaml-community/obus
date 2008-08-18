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

     let w = wait () in

     notify ~summary:"Actions test" ~body:"click on something!"
       ~category:"network"
       ~actions:[("coucou", fun _ -> print_endline "You pressed coucou!");
                 ("plop", fun _ -> print_endline "You pressed plop!")]
       ~on_close:(fun _ -> print_endline "popup closed")
       ~wakeup:(wakeup w) ();

     (* Wait for the notification to be closed *)
     _ <-- w;

     return ())

let _ = Lwt_unix.run main
