(*
 * util.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

val with_open_in : string -> (in_channel -> 'a) -> 'a
val with_open_out : string -> (out_channel -> 'a) -> 'a

val join : string list -> string -> string
val rjoin : string list -> string -> string
val split_upper : string -> string list
