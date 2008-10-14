(*
 * auth.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt_unix
open Lwt_chan
open Lwt
open OBus_auth

let chans () =
  let fdr, fdw = pipe () in
  (in_channel_of_descr fdr,
   out_channel_of_descr fdw)

let _ =
  let server_ic, client_oc = chans ()
  and client_ic, server_oc = chans () in
  run (Lwt_util.join [(perform
                         guid <-- client_authenticate (client_ic, client_oc);
                         let _ = Printf.printf "client: authenticated, guid=%s\n%!" (OBus_uuid.to_string guid) in
                         return ());
                      (perform
                         server_authenticate ~mechanisms:[server_mech_dbus_cookie_sha1]
                           (OBus_uuid.generate ()) (server_ic, server_oc);
                         let _ = print_endline "server: client authenticated" in
                         return ())])
