(*
 * unixT.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Unix transport for OBus *)

class transport : Unix.file_descr -> object
  inherit Transport.t
  method backend : [ `Unix of Unix.file_descr ]
    (** File descriptor associated with the connection *)
end
