(*
 * test_validation.ml
 * ------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open Lwt_io

let good = [
  OBus_string.validate "azerty";
  OBus_string.validate "Jérémie";

  OBus_path.validate "/";
  OBus_path.validate "/a";
  OBus_path.validate "/a/b";

  OBus_name.validate_bus ":1.1";
  OBus_name.validate_bus ":a.2";
  OBus_name.validate_bus "foo.bar";
  OBus_name.validate_bus "a.b.c.d";
]

let bad = [
  OBus_string.validate "\xe9";

  OBus_path.validate "/dd//dd";
  OBus_path.validate "/dd//";
  OBus_path.validate "/dd/";
  OBus_path.validate "";

  OBus_name.validate_bus ":1..2";
  OBus_name.validate_bus "a..b";
]

let test () =
  let%lwt () = printl "Validation of all types of D-Bus strings" in
  let%lwt () =
    Lwt_list.iter_s
      (function
         | Some err ->
             printlf "valid string recognized as bad: %s" (OBus_string.error_message err)
         | None ->
             return ())
      good
  in
  let%lwt () =
    Lwt_list.iter_s
      (function
         | None ->
             printlf "invalid string recognized as good"
         | Some _ ->
             return ())
      bad
  in
  return (List.for_all ((=) None) good && List.for_all ((<>) None) bad)
