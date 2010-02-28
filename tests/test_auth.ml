(*
 * test_auth.ml
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Log = Lwt_log.Make(struct let section = "" end)

open Lwt

let server_ic, client_oc = Lwt_io.pipe ()
let client_ic, server_oc = Lwt_io.pipe ()

let guid = OBus_uuid.generate ()
let user_id = Unix.getuid ()

let test mech =
  try_lwt
    lwt () = Lwt.join [OBus_auth.Client.authenticate
                         ~stream:(OBus_auth.stream_of_channels (client_ic, client_oc)) ()
                       >> return ();
                       OBus_auth.Server.authenticate
                         ~user_id
                         ~mechanisms:[mech]
                         ~guid
                         ~stream:(OBus_auth.stream_of_channels (server_ic, server_oc)) ()
                       >> return ()] in
    Lwt_io.eprintlf "authentication %s works!" (OBus_auth.Server.mech_name mech)
  with exn ->
    lwt () = Log.exn_f exn "authentication %s do not works" (OBus_auth.Server.mech_name mech) in
    return ()

lwt () =
  lwt () = test OBus_auth.Server.mech_external in
  lwt () = test OBus_auth.Server.mech_dbus_cookie_sha1 in
  lwt () = test OBus_auth.Server.mech_anonymous in
  return ()
