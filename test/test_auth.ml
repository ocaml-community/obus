(*
 * test_auth.ml
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

let server_ic, client_oc = Lwt_io.pipe ()
let client_ic, server_oc = Lwt_io.pipe ()

let guid = OBus_uuid.generate ()

let test mech =
  try_lwt
    lwt () = Lwt_util.join [OBus_auth.Client.authenticate (OBus_auth.stream_of_channels client_ic client_oc) >> return ();
                            OBus_auth.Server.authenticate ~mechanisms:[mech] guid (OBus_auth.stream_of_channels server_ic server_oc)] in
    Lwt_io.eprintlf "authentication %s works!" (fst mech)
  with _ ->
    Lwt_io.eprintlf "authentication %s do not works :(" (fst mech)

let () = Lwt_main.run (test OBus_auth.Server.mech_dbus_cookie_sha1)
