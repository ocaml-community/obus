(*
 * xml_parser.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

(** Facilities for parsing xml *)

exception Parse_failed

type 'a xml_parser

type ('a, 'b) param_parser
  (** A parser of parameters list which return a value of type 'b *)

type 'a seq_elt_parser
  (** A parser of a list of xml, wich known how to parse just one
      element type and return a value of type 'a *)

type ('a, 'b) seq_parser
  (** A parser of a list of xml which return a value of type 'b *)

val pc : string -> ('a, 'b) param_parser -> (string -> 'a, 'b) param_parser
val pn : ('a, 'a) param_parser
  (** Construction of parameters parsers *)

val sc : 'a seq_elt_parser -> ('b, 'c) seq_parser -> ('a -> 'b, 'c) seq_parser
val sn : ('a, 'a) seq_parser
  (** Construction of list of xml parsers *)

val one : 'a xml_parser -> 'a seq_elt_parser
  (** [one parser] parse one element of a given type in a list of
      xml *)

val any : 'a xml_parser -> 'a list seq_elt_parser
  (** [any parser] parse any element of a given type in a list of
      xml *)

val opt : 'a xml_parser -> 'a option seq_elt_parser
  (** [opt parser] parse at most one element of a given type in a list
      of xml *)

val elt : string -> ('a, 'b) param_parser -> ('b, 'c) seq_parser -> 'a -> 'c xml_parser
  (** [elt name params sons f] parse an element of type [name] with
      parameters [params] and sons [sons] and apply [f] on the
      result *)

val parse : 'a xml_parser -> Xml.xml -> 'a
  (** [parse parser xml] Parse an entire xml document using
      [parser] *)
