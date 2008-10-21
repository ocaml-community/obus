(*
 * logging.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module Log = Log.Make(struct let section = "test" end)

let _ =
  OBus_info.verbose := true;
  OBus_info.debug := true;
  Log.log "plop %s" "plip";
  Log.debug "toto";
  Log.error "fatal error";
  IFDEF HAVE_BACKTRACE THEN
    Printexc.record_backtrace false;
  END;
  (try
     if 1 = 1 then
       raise (Failure "arg!")
   with
       exn -> Log.failure exn "");
  IFDEF HAVE_BACKTRACE THEN
    Printexc.record_backtrace true;
  END;
  try
    if 1 = 1 then
      raise (Failure "arg!")
  with
      exn -> Log.failure exn "something failed with"
