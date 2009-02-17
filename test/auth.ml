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

let server_ic, client_oc = chans ()
let client_ic, server_oc = chans ()

let guid = OBus_uuid.generate ()

let test mech =
  catch
    (fun _ -> perform
       Lwt_util.join [(perform
                         guid <-- client_authenticate (OBus_auth.stream_of_lwt_channels (client_ic, client_oc));
                         return ());
                      server_authenticate ~mechanisms:[mech] guid (OBus_auth.stream_of_lwt_channels (server_ic, server_oc))];
       let _ = Printf.eprintf "authentication %s works!\n%!" (fst mech) in
       return ())
    (fun _ ->
       Printf.eprintf "authentication %s do not works :(\n%!" (fst mech);
       return ())

let _ =
  run (test server_mech_dbus_cookie_sha1)
