(*
 * valid.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_name
open OBus_type.Perv
open Lwt

let test f s =
  match f s with
    | Some err ->
        prerr_endline (OBus_string.error_message err)
    | None ->
        ()

let testc b s =
  catch (fun _ -> OBus_connection.method_call b
           ~interface:"toto"
           ~member:"toto"
           ~path:[]
           <:obus_func< string -> unit >> s)
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
  Lwt_main.run (
    lwt b = Lazy.force OBus_bus.session in
    lwt () = testc b "aa\x81oo" in
    lwt () = testc b "dfsdf\x00kljsd" in
    OBus_connection.method_call b
      ~interface:"aa.$.e"
      ~member:"toto"
      ~path:[]
      <:obus_func< unit >>
  )
