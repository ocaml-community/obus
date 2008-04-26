(*
 * dumper.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)


val make : string -> OBus.Transport.t -> OBus.Transport.t
  (** [make prefix transport] make a transport which use the given
      transport and dump everything *)
