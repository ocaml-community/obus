(*
 * lock.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

class lock = object
  method acquire = ()
  method release = ()
end

let create_function =
  ref (fun () -> new lock)

let create () = !create_function ()
