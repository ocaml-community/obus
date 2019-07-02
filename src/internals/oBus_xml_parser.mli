(*
 * oBus_xml_parser.mli
 * -------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Monadic xml parsing *)

(** This module implements a simple monadic xml parser.

    It is intended to make it easy to write XML document parsers. In
    OBus it is used to parse introspection document. *)

exception Parse_failure of Xmlm.pos * string

type xml_parser
  (** Type of an xml parser. It is used to parse a sequence of
      arguments and children of an element. *)

type 'a node
  (** Type of a single xml node parser, returning a value of type
      ['a] *)

val failwith : xml_parser -> string -> 'a
  (** Fail at current position with the given error message *)

val input : Xmlm.input -> 'a node -> 'a
  (** Run a parser on a xml input. If it fails it raises a
      [Parse_failure] *)

(** {6 Parsing of attributes} *)

(** For the following functions, the first argument is the attribute
    name and each letter mean:

    - [o] : the attribute is optionnal
    - [r] : the attribute is required
    - [d] : a default value is given
    - [f] : a associative list  for the attribute value is specified. *)

val ar : xml_parser -> string -> string
val ao : xml_parser -> string -> string option
val ad : xml_parser -> string -> string -> string
val afr : xml_parser -> string -> (string * 'a) list -> 'a
val afo : xml_parser -> string -> (string * 'a) list -> 'a option
val afd : xml_parser -> string -> 'a -> (string * 'a) list -> 'a

(** {6 Parsing of elements} *)

val elt : string -> (xml_parser -> 'a) -> 'a node
  (** [elt typ parser] creates a node parser. It will parse element of
      type [typ]. [parser] is used to parse the attributes and
      children of the element.

      Note that [parser] must consume all children, if some are left
      unparsed the parsing will fail. *)

val pcdata : string node
  (** [pcdata f] parse one PCData *)

val map : 'a node -> ('a -> 'b) -> 'b node
  (** [map node f] wraps the result of a node parser with [f] *)

val union : 'a node list -> 'a node
  (** [union nodes] Node parser which parses any node matched by one of
      the given node parsers *)

(** {6 Modifiers} *)

val one : xml_parser -> 'a node -> 'a
  (** [one node] parse exactly one node with the given node parser. It
      will fail if there is 0 or more than one node matched by
      [node]. *)

val opt : xml_parser -> 'a node -> 'a option
  (** same as [one] but do not fail if there is no node matched by
      [node]. *)

val any : xml_parser -> 'a node -> 'a list
  (** [any node] Parse all element matched by [node]. The resulting
      list is in the same order as the order in which nodes appears in
      the xml. *)
