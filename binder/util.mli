(*
 * util.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

include Common_util.S

val with_open_in : string -> (in_channel -> 'a) -> 'a
val with_open_out : string -> (out_channel -> 'a) -> 'a

val ljoin : string -> string list -> string
val rjoin : string -> string list -> string
val split_upper : string -> string list
val dot_regexp : Str.regexp
val newline_regexp : Str.regexp
val split_dot : string -> string list
val split_lines : string -> string list

val part_map : ('a -> 'b option) -> 'a list -> 'b list * 'a list

val parse_xml : string -> Xml.xml

val gen_names : string -> 'a list -> string list

val gen_list : (int -> 'a) -> int -> int -> 'a list
