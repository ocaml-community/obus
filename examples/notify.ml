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
    ~actions:[("coucou", `coucou);
              ("plop", `plop)] ()
  in

  lwt result = notif#result in

  begin match result with
    | `coucou -> print_endline "You pressed coucou!"
    | `plop -> print_endline "You pressed plop!"
    | `default -> print_endline "default action invoked"
    | `closed -> print_endline "notification closed"
  end;
  return ()
end
