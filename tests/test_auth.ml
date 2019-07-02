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
let user_id = Unix.getuid ()

let test_mech mech =
  try%lwt
    let%lwt () = Lwt.join
      [(let%lwt _ = OBus_auth.Client.authenticate
                    ~stream:(OBus_auth.stream_of_channels (client_ic, client_oc)) () in
       return ());
       let%lwt _ = OBus_auth.Server.authenticate
                     ~user_id
                     ~mechanisms:[mech]
                     ~guid
                     ~stream:(OBus_auth.stream_of_channels (server_ic, server_oc)) () in
       return ()] in
    let%lwt () = Lwt_io.printlf "authentication %s works!" (OBus_auth.Server.mech_name mech) in
    return true
  with exn ->
    let%lwt () = Lwt_io.printlf "authentication %s do not works: %s" (OBus_auth.Server.mech_name mech) (Printexc.to_string exn) in
    return false

let test () =
  let%lwt a = test_mech OBus_auth.Server.mech_external in
  let%lwt b = test_mech OBus_auth.Server.mech_dbus_cookie_sha1 in
  let%lwt c = test_mech OBus_auth.Server.mech_anonymous in
  return (a && b && c)
