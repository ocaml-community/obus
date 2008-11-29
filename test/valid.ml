(*
 * valid.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_name
open OBus_type
open Lwt

let test f s =
  match f s with
    | Some err ->
        prerr_endline (OBus_string.error_message err)
    | None ->
        ()

let testc b s =
  catch (fun _ -> OBus_connection.method_call (OBus_bus.connection b)
           ~interface:"toto"
           ~member:"toto"
           ~path:[]
           << string -> unit >> s)
    (fun exn -> prerr_endline (Printexc.to_string exn); return ())

let _ =
  test OBus_path.test "";
  test OBus_path.test "/";
  test OBus_path.test "/dd//dd";
  test OBus_path.test "/dd//";
  test OBus_path.test "/dd/";
  test test_connection ":er.1dsf";
  test test_connection ":er..dsf";
  test test_connection "erdsf.1ze";
  test test_interface "toto";
  Lwt_unix.run
    (perform
       b <-- Lazy.force OBus_bus.session;
       testc b "aa\x81oo";
       testc b "dfsdf\x00kljsd";
       OBus_connection.method_call (OBus_bus.connection b)
         ~interface:"aa.$.e"
         ~member:"toto"
         ~path:[]
       << unit >>)
