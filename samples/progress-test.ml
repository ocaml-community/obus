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

let service = "org.ocamlcore.forge.obus.ProgressBar"

module Bar =
struct
  include OBus_interface.Make(struct let name = "org.ocamlcore.forge.obus.ProgressBar.Bar" end)
  OBUS_property_rw Position : int
  OBUS_method Close : unit
  OBUS_signal Closed : closed_reason
end

module Manager =
struct
  include OBus_interface.Make(struct let name = "org.ocamlcore.forge.obus.ProgressBar.Manager" end)
  let path = ["org"; "ocamlcore"; "forge"; "obus"; "ProgressBar"; "Manager"]

  OBUS_method ServerVersion : string
  OBUS_method CreateProgressBar : int -> Bar.t
end

let _ = Random.self_init ()

let rec test bar = function
  | 0 -> return ()
  | n ->
      (perform
         p <-- OBus_property.get bar Bar.position;
         let _ = Printf.printf "position: %d\n%!" p in
         OBus_property.set bar Bar.position (Random.int 101);
         Lwt_unix.sleep 0.5;
         test bar (n - 1))

let _ = Lwt_unix.run
  (perform
     bus <-- Lazy.force OBus_bus.session;
     let manager = OBus_bus.make_proxy bus service Manager.path in

     ver <-- Manager.server_version manager;
     let _ = Printf.printf "server version: %s\n" ver in

     bar <-- Manager.create_progress_bar manager 10;
     OBus_signal.connect bar Bar.closed
       (fun reason ->
          begin match reason with
            | Cancel -> print_endline "canceled"
            | OK -> print_endline "OK clicked"
            | Explicitly_closed -> print_endline "closed"
            | Killed -> print_endline "killed"
          end;
          exit 0);

     test bar 10)
