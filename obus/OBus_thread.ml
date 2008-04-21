(*
 * OBus_thread.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

class lock = object
  val m = Mutex.create ()
  method acquire = Mutex.lock m
  method release = Mutex.unlock m
end

let init () = Lock.create_function := (fun () -> new lock)
