(*
 * uPower_wakeups.mli
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

val get_data : UPower.t -> (bool * int * float * string * string) list Lwt.t
val get_total : UPower.t -> int Lwt.t
val data_changed : UPower.t -> unit OBus_proxy.signal
val total_changed : UPower.t -> int OBus_proxy.signal
val has_capability : UPower.t -> bool Lwt.t
