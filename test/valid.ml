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
  try
    f s
  with exn -> prerr_endline (Printexc.to_string exn)

let testc b s =
  catch (fun _ -> OBus_connection.method_call b
           ~interface:"toto"
           ~member:"toto"
           ~path:"/"
           << string -> unit >> s)
    (fun exn -> prerr_endline (Printexc.to_string exn); return ())

let _ =
  test OBus_path.validate "";
  test OBus_path.validate "/";
  test OBus_path.validate "/dd//dd";
  test OBus_path.validate "/dd//";
  test OBus_path.validate "/dd/";
  test Connection.validate ":er.1dsf";
  test Connection.validate ":er..dsf";
  test Connection.validate "erdsf.1ze";
  Lwt_unix.run
    (perform
       b <-- Lazy.force OBus_bus.session;
       testc b "aa\x81oo";
       testc b "dfsdf\x00kljsd")
