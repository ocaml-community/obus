(*
 * parser.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Parsing of introspection data *)

open Xparser
open Introspect

val document : interface list xml_parser
  (** Parser for a whole document *)
