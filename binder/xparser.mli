(*
 * xparser.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Facilities for parsing xml *)

exception Parse_failed

type 'a xml_parser

type ('a, 'b) param_parser = (string, string, 'a, 'b) Seq.t
  (** A parser of parameters list which return a value of type 'b *)

type 'a seq_elt_parser
  (** A parser of a list of xml, wich known how to parse just one
      element type and return a value of type 'a *)

type ('a, 'b) seq_parser
  (** A parser of a list of xml which return a value of type 'b *)

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

val union : 'a xml_parser list -> 'a list seq_elt_parser
  (** [union parsers] parse any element of one of the types parsed by
      [parsers]. In the resulting list element are in the same order
      as in the xml *)

val elt : string -> ('a, 'b) param_parser -> ('b, 'c) seq_parser -> 'a -> 'c xml_parser
  (** [elt name params sons f] parse an element of type [name] with
      parameters [params] and sons [sons] and apply [f] on the
      result *)

val parse : 'a xml_parser -> Xml.xml -> 'a
  (** [parse parser xml] Parse an entire xml document using
      [parser] *)

val s0 : ('a, 'a) seq_parser
val s1 : 'a1 seq_elt_parser -> ('a1 -> 'a, 'a) seq_parser
val s2 : 'a2 seq_elt_parser -> 'a1 seq_elt_parser -> ('a2 -> 'a1 -> 'a, 'a) seq_parser
val s3 : 'a3 seq_elt_parser -> 'a2 seq_elt_parser -> 'a1 seq_elt_parser -> ('a3 -> 'a2 -> 'a1 -> 'a, 'a) seq_parser
val s4 : 'a4 seq_elt_parser -> 'a3 seq_elt_parser -> 'a2 seq_elt_parser -> 'a1 seq_elt_parser -> ('a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) seq_parser
val s5 : 'a5 seq_elt_parser -> 'a4 seq_elt_parser -> 'a3 seq_elt_parser -> 'a2 seq_elt_parser -> 'a1 seq_elt_parser -> ('a5 -> 'a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) seq_parser
