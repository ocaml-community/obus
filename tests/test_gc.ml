(*
 * test_gc.ml
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open Lwt_io

let ok = ref false
let finalise _  = ok := true

let test () =
  let success = true in
  let%lwt bus = OBus_bus.session () in

  let%lwt () = print "safety check: " in
  let event = ref 0 in
  ok := false;
  Gc.finalise finalise event;
  let event = 1 in
  ignore event;
  Gc.full_major ();
  let%lwt () = printl (if !ok then "success" else "failure") in
  let success = success && !ok in

  let%lwt () = print "testing garbage collection of a signal without a switch: " in
  let%lwt event = OBus_signal.connect (OBus_bus.name_owner_changed bus) in
  ok := false;
  Gc.finalise finalise event;
  let event = 1 in
  ignore event;
  Gc.full_major ();
  let%lwt () = printl (if !ok then "success" else "failure") in
  let success = success && !ok in

  let%lwt () = print "testing garbage collection of a signal with a switch: " in
  let switch = Lwt_switch.create () in
  let%lwt event = OBus_signal.connect ~switch (OBus_bus.name_owner_changed bus) in
  ok := false;
  Gc.finalise finalise event;
  let event = 1 in
  ignore event;
  Gc.full_major ();
  let%lwt () = printl (if !ok then "success" else "failure") in
  let success = success && !ok in

  return success
