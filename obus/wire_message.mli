(*
 * wireMessage.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Module used to receive or send an entire message *)

val recv : Lwt_chan.in_channel -> OBus_message.any Lwt.t
val send : Lwt_chan.out_channel -> 'a OBus_message.t -> unit Lwt.t
