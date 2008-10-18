(*
 * progress-test.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type

OBUS_flag closed_reason : uint =
    | 0 -> Cancel
    | 1 -> OK
    | 2 -> Explicitly_closed
    | 3 -> Killed

module Bar =
struct
  include OBus_client.Make(struct let name = "org.ocamlcore.forge.obus.ProgressBar.Bar" end)
  let tt = OBus_proxy.tt
  let position = property "Position" OBus_property.rdwr <:obus_type< int >>
  let close = call "Close" << unit >>
  let on_closed = on_signal "Closed" <:obus_type< closed_reason >>
end

module Manager =
struct
  include OBus_client.Make_constant
    (struct
       let name = "org.ocamlcore.forge.obus.ProgressBar.Manager"
       let service = Some "org.ocamlcore.forge.obus.ProgressBar"
       let path = ["org"; "ocamlcore"; "forge"; "obus"; "ProgressBar"; "Manager"]
       let bus = OBus_bus.session
     end)

  let server_version = call "ServerVersion" << unit -> string >>
  let create_progress_bar = call "CreateProgressBar" << int -> Bar.t >>
end

let _ = Random.self_init ()

let rec test bar = function
  | 0 -> return ()
  | n ->
      (perform
         p <-- OBus_property.get (Bar.position bar);
         let _ = Printf.printf "position: %d\n%!" p in
         OBus_property.set (Bar.position bar) (Random.int 101);
         Lwt_unix.sleep 0.5;
         test bar (n - 1))

let _ = Lwt_unix.run
  (perform
     bus <-- Lazy.force OBus_bus.session;

     ver <-- Manager.server_version ();
     let _ = Printf.printf "server version: %s\n" ver in

     bar <-- Manager.create_progress_bar 10;
     Bar.on_closed bar (fun reason ->
                          begin match reason with
                            | Cancel -> print_endline "canceled"
                            | OK -> print_endline "OK clicked"
                            | Explicitly_closed -> print_endline "closed"
                            | Killed -> print_endline "killed"
                          end;
                          exit 0);

     test bar 10)
