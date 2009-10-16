(*
 * logging.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let _ =
  OBus_info.verbose := true;
  OBus_info.debug := true;
  OBus_log.log ~section:"test" "plop %s" "plip";
  OBus_log.debug ~section:"test" "toto";
  OBus_log.error ~section:"test" "fatal error";
  Printexc.record_backtrace false;
  begin
    try
      if 1 = 1 then
        raise (Failure "arg!")
    with
        exn -> OBus_log.failure exn ""
  end;
  Printexc.record_backtrace true;
  begin
    try
      if 1 = 1 then
        raise (Failure "arg!")
    with
        exn -> OBus_log.failure exn "something failed with"
  end
