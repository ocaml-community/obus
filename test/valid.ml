(*
 * valid.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_name
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
  test OBus_path.validate "";
  test OBus_path.validate "/";
  test OBus_path.validate "/dd//dd";
  test OBus_path.validate "/dd//";
  test OBus_path.validate "/dd/";
  test validate_bus ":er.1dsf";
  test validate_bus ":er..dsf";
  test validate_bus "erdsf.1ze";
  test validate_bus "toto";
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
