(*
 * buf.mli
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

val put : ?byte_order:OBus_lowlevel.byte_order -> OBus_message.t -> string
  (** [put ?byte_order message] marshal [msg] into a string *)

val get : string -> OBus_message.t
  (** [get str] unmarshal a message from a string *)
