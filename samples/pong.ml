(*
 * pong.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Very simple service with one object have a ping method *)

open Lwt
open OBus_type

OBUS_class pong "org.plop.foo" = object
  OBUS_method ping : string -> string
end

let obj = object(self)
  inherit OBus_object.t
  inherit pong

  method ping m = return ("pong in reply to: " ^ m)
end

let _ = Lwt_unix.run
  (perform
     bus <-- Lazy.force OBus_bus.session;

     (* Request a name *)
     OBus_bus.request_name bus "org.plop" [];

     (* Expose the object on the connection *)
     let _ = obj#obus_export bus ["plip"] in

     (* Wait forever *)
     wait ())
