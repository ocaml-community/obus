(*
 * messageRW.mli
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Internal module for marshaling/unmarshaling messages *)

val read : Transport.t -> string ref -> Header.recv Header.t * int
val write : Transport.t -> string ref -> Header.send Header.t -> Header.serial -> (Header.byte_order -> string -> int -> int) -> unit
