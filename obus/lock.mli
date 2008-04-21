(*
 * lock.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Lock handling for threaded version *)

class lock : object
  method acquire : unit
  method release : unit
end

val create_function : (unit -> lock) ref
  (** [create_function] function used for creating lock. By default it
      create lock that do nothing *)

val create : unit -> lock
  (** [create ()] create a lock *)
