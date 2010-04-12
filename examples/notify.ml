(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

let _ = Lwt_main.run begin
  lwt _ = Notification.notify ~summary:"Hello, world!" ~body:"ocaml is fun!" ~icon:"info" () in
  lwt () = Lwt_unix.sleep 0.5 in

  lwt notif = Notification.notify ~summary:"Actions test" ~body:"click on something!"
    ~category:"network"
    ~actions:[("coucou", `Coucou);
              ("plop", `Plop)] ()
  in

  lwt result = Notification.result notif in

  begin match result with
    | `Coucou -> print_endline "You pressed coucou!"
    | `Plop -> print_endline "You pressed plop!"
    | `Default -> print_endline "default action invoked"
    | `Closed -> print_endline "notification closed"
  end;
  return ()
end
