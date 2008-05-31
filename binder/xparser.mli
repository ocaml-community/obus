(*
 * xparser.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Facilities for parsing xml *)

type 'a xml_parser

(** {6 Parsing attributes} *)

type 'a attribute_parser
  (** A attribute parser which return an value of type ['a] *)

type ('a, 'b) attributes_parser
  (** A parser for a list of attributes *)

val ac : 'a attribute_parser -> ('b, 'c) attributes_parser -> ('a -> 'b, 'c) attributes_parser
val an : ('a, 'a) attributes_parser
  (** Construction of attribute list parsers *)

(** For the following functions, the first argument is the attribute
    name and each letter mean:

    - [o] : the attribute is optionnal
    - [r] : the attribute is required
    - [d] : a default value is given
    - [f] : a field for the attribute value is specified
    - [s] : the attribute value is a string, which will not be processed *)

val a : string -> (string option -> 'a) -> 'a attribute_parser
  (** [a name f] most general attribute parser. If the attribute is
      present then its value is passed to [f], otherwise [None] is
      passed to [f] *)

val ar : string -> (string -> 'a) -> 'a attribute_parser
val ao : string -> (string -> 'a) -> 'a option attribute_parser
val ad : string -> 'a -> (string -> 'a) -> 'a attribute_parser
val afr : string -> (string * 'a) list -> 'a attribute_parser
val afo : string -> (string * 'a) list -> 'a option attribute_parser
val afd : string -> 'a -> (string * 'a) list -> 'a attribute_parser
val ars : string -> string attribute_parser
val aos : string -> string option attribute_parser
val ads : string -> string -> string attribute_parser
val afrs : string -> string list -> string attribute_parser
val afos : string -> string list -> string option attribute_parser
val afds : string -> string -> string list -> string attribute_parser

(** {6 Parsing element} *)

type 'a seq_elt_parser
  (** A parser of a list of xml, wich known how to parse just one
      element type and return a value of type ['a] *)

type ('a, 'b) seq_parser
  (** A parser of a list of xml which return a value of type ['b] *)

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

val elt : string -> ('a, 'b) attributes_parser -> ('b, 'c) seq_parser -> 'a -> 'c xml_parser
  (** [elt name attributes sons f] parse an element of type [name] with
      attributes [attributes] and sons [sons] and apply [f] on the
      result. *)

val pcdata : (string -> 'a) -> 'a xml_parser
  (** [pcdata f] parse a PCData and apply f on its content *)

val text : string xml_parser
  (** [text] equivalent of [pcdata (fun x -> x)] *)

(** {6 Parsing a xml document} *)

val parse : 'a xml_parser -> ?filename:string -> Xml.xml -> 'a
  (** [parse parser filename xml] Parse an entire xml document using
      [parser] *)

(** {6 Helpers} *)

val s0 : ('a, 'a) seq_parser
val s1 : 'a1 seq_elt_parser -> ('a1 -> 'a, 'a) seq_parser
val s2 : 'a2 seq_elt_parser -> 'a1 seq_elt_parser -> ('a2 -> 'a1 -> 'a, 'a) seq_parser
val s3 : 'a3 seq_elt_parser -> 'a2 seq_elt_parser -> 'a1 seq_elt_parser -> ('a3 -> 'a2 -> 'a1 -> 'a, 'a) seq_parser
val s4 : 'a4 seq_elt_parser -> 'a3 seq_elt_parser -> 'a2 seq_elt_parser -> 'a1 seq_elt_parser -> ('a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) seq_parser
val s5 : 'a5 seq_elt_parser -> 'a4 seq_elt_parser -> 'a3 seq_elt_parser -> 'a2 seq_elt_parser -> 'a1 seq_elt_parser -> ('a5 -> 'a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) seq_parser
val a0 : ('a, 'a) attributes_parser
val a1 : 'a1 attribute_parser -> ('a1 -> 'a, 'a) attributes_parser
val a2 : 'a2 attribute_parser -> 'a1 attribute_parser -> ('a2 -> 'a1 -> 'a, 'a) attributes_parser
val a3 : 'a3 attribute_parser -> 'a2 attribute_parser -> 'a1 attribute_parser -> ('a3 -> 'a2 -> 'a1 -> 'a, 'a) attributes_parser
val a4 : 'a4 attribute_parser -> 'a3 attribute_parser -> 'a2 attribute_parser -> 'a1 attribute_parser -> ('a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) attributes_parser
val a5 : 'a5 attribute_parser -> 'a4 attribute_parser -> 'a3 attribute_parser -> 'a2 attribute_parser -> 'a1 attribute_parser -> ('a5 -> 'a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) attributes_parser
val a6 : 'a6 attribute_parser -> 'a5 attribute_parser -> 'a4 attribute_parser -> 'a3 attribute_parser -> 'a2 attribute_parser -> 'a1 attribute_parser -> ('a6 -> 'a5 -> 'a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) attributes_parser
val a7 : 'a7 attribute_parser -> 'a6 attribute_parser -> 'a5 attribute_parser -> 'a4 attribute_parser -> 'a3 attribute_parser -> 'a2 attribute_parser -> 'a1 attribute_parser -> ('a7 -> 'a6 -> 'a5 -> 'a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) attributes_parser
val a8 : 'a8 attribute_parser -> 'a7 attribute_parser -> 'a6 attribute_parser -> 'a5 attribute_parser -> 'a4 attribute_parser -> 'a3 attribute_parser -> 'a2 attribute_parser -> 'a1 attribute_parser -> ('a8 -> 'a7 -> 'a6 -> 'a5 -> 'a4 -> 'a3 -> 'a2 -> 'a1 -> 'a, 'a) attributes_parser
