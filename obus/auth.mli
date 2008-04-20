(*
 * auth.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type auth_result =
  | Success
  | Failure of string

val do_auth : Unix.file_descr -> auth_result
  (** [do_auth fd] run authentification mechanism on the given
      transport *)
