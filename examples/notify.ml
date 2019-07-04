(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open Lwt_io

let () = Lwt_main.run begin
  (* Open a first notification: *)
  let%lwt _ = Notification.notify ~summary:"Hello, world!" ~body:"ocaml is fun!" ~icon:"info" () in

  let%lwt () = Lwt_unix.sleep 0.5 in

  (* Open another one, with buttons on it: *)
  let%lwt handle =
    Notification.notify ~summary:"Actions test" ~body:"click on something!"
      ~category:"network"
      ~actions:[("coucou", `Coucou); ("plop", `Plop)] ()
  in

  (* Then wait for the result: *)
  Notification.result handle >>= function
    | `Coucou -> eprintl "You pressed coucou!"
    | `Plop -> eprintl "You pressed plop!"
    | `Default -> eprintl "default action invoked"
    | `Closed -> eprintl "notification closed"
end
