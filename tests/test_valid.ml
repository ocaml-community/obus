(*
 * test_valid.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_name
open OBus_pervasives
open Lwt

let test f s =
  match f s with
    | Some err ->
        prerr_endline (OBus_string.error_message err)
    | None ->
        ()

let testc bus str =
  try_lwt
    OBus_connection.method_call bus
      ~interface:"toto"
      ~member:"toto"
      ~path:[]
      <:obus_func< string -> unit >> str
  with exn ->
    Lwt_log.error ~exn "error"

lwt () =
  test OBus_path.validate "";
  test OBus_path.validate "/";
  test OBus_path.validate "/dd//dd";
  test OBus_path.validate "/dd//";
  test OBus_path.validate "/dd/";
  test validate_bus ":er.1dsf";
  test validate_bus ":er..dsf";
  test validate_bus "erdsf.1ze";
  test validate_bus "toto";
  lwt bus = OBus_bus.session () in
  lwt () = testc bus "aa\x81oo" in
  lwt () = testc bus "dfsdf\x00kljsd" in
  OBus_connection.method_call bus
    ~interface:"aa.$.e"
    ~member:"toto"
    ~path:[]
    <:obus_func< unit >>
