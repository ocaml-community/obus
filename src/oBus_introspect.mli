(*
 * oBus_introspect.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Introspection *)

type name = string

type annotation = name * string
type argument = name option * OBus_value.tsingle

type access = Read | Write | Read_write
    (** Access mode of properties *)

type declaration =
  | Method of name * argument list * argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * OBus_value.tsingle * access * annotation list

type interface = name * declaration list * annotation list
type node = OBus_path.element

type document = interface list * node list with obus(basic)

(** {6 Xml conversion} *)

type parsing_error

exception Parse_failure of parsing_error

val print_error : Format.formatter -> parsing_error -> unit
  (** Print a parsing error with the given formatter. It can take
      multiple lines. *)

val of_xml : Xml.xml -> document
  (** Try to read an xml document as an introspection document.

      @raise Parse_failure if the parsing fail. *)

val to_xml : document -> Xml.xml
  (** Create an xml from an introspection document*)
