(*
 * oBus_introspect.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Introspection document *)

(** {6 Structure of an introspection document} *)

type name = string

type annotation = name * string
type argument = name option * OBus_types.single

type access = Read | Write | Read_write
    (** Access mode of properties *)

type declaration =
  | Method of name * argument list * argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * OBus_types.single * access * annotation list

type interface = name * declaration list * annotation list
type node = name
type document = interface list * node list

(** {6 Parsers} *)

module Make_parser(Xml_parser : OBus_xml_parser.S) : sig
  val interface : interface Xml_parser.node
  val node : node Xml_parser.node
  val document : document Xml_parser.node
end
