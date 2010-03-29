(*
 * network_state.ml
 * ----------------
 * Copyright : (c) 2010, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* simple example showing how to monitor network state *)

open Lwt
open React
open NetworkManager

let print s =
  Lwt_io.printf "\r\027[2K%s" s

let () =
  Lwt_main.run
    begin
      lwt t = Lazy.force t in
      lwt state = state t in
      let state_string = S.map string_of_state state#signal in
      let () = Lwt_signal.always_notify_s print state_string in
      let t,w = wait () in
      t
    end
