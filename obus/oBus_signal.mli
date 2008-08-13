(*
 * oBus_signal.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Handling of signals *)

type id = OBus_internals.signal_id
  (** Id of a registred callback function *)

val disconnect : id -> unit
  (** Remove a previously registred callback function. It do nothing
      if the callback function has already been disconnected *)
