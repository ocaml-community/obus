(*
 * binder.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

module MakeMap(L : Language.S) :
sig
  val main : unit -> unit
end

module MakeGen(L : Language.S) :
sig
  val main : unit -> unit
end

