(*
 * oBus_xml_parser.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Monadic xml parsing *)

(** This module implement a simple monadic xml parser.

    It is intended to make it easy to write XML document parsers. In
    OBus it is used to parse introspection document. *)

(** Needed function *)
module type XML = sig
  type t
  val match_node :
    element:(string -> (string * string) list -> t list -> 'a) ->
    pcdata:(string -> 'a) -> t -> 'a
end

type attributes = (string * string) list
type stack = (string * attributes) list
    (** The stack represent the location of an error. It is a list of
        (element name, attributes). *)

val print_stack : Format.formatter -> stack -> unit
  (** Print a stack.

      It produce something like that:

      <foo name="toto" value="x">
      <bar arg="1">
      ...
  *)

module type S = sig
  type xml

  type 'a t
    (** Type of an xml parser *)

  type 'a node
    (** Type of a single node parser *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    (** Monad operations *)

  (** Note that it is not possible to catch errors during parsing,
      error are reported only by the [run] operation. *)

  val failwith : string -> 'a t
    (** Fail at current position with this error message *)

  val parse : 'a node -> xml -> [ `Right of 'a | `Left of stack * string ]
    (** Run a parser on an xml document. If it fail it return the
        stack of error and an error message. *)

  (** {6 Parsing of attributes} *)

  (** For the following functions, the first argument is the attribute
      name and each letter mean:

      - [o] : the attribute is optionnal
      - [r] : the attribute is required
      - [d] : a default value is given
      - [f] : a associative list  for the attribute value is specified. *)

  val ar : string -> string t
  val ao : string -> string option t
  val ad : string -> string -> string t
  val afr : string -> (string * 'a) list -> 'a t
  val afo : string -> (string * 'a) list -> 'a option t
  val afd : string -> 'a -> (string * 'a) list -> 'a t

  (** {6 Parsing of elements} *)

  val elt : string -> 'a t -> 'a node
    (** [elt typ parser] Create a node parser . It will parse element
        of type [typ]. [parser] is used to parse the attributes and
        children of the element.

        Note that [parser] must consume all children, if some are left
        unparsed the parsing will fail. *)

  val pcdata : string node
    (** [pcdata f] parse one PCData *)

  val raw : xml node
    (** Return an unparsed node. [any raw] will always consume all the
        input. *)

  val wrap : 'a node -> ('a -> 'b) -> 'b node
    (** [wrap node f] wrap the result of a node parser with [f] *)

  val union : 'a node list -> 'a node
    (** [union nodes] Node parser which parse any node matched by one
        of the given node parsers *)

  (** {6 Modifiers} *)

  val one : 'a node -> 'a t
    (** [one node] parse exactly one node with the given node
        parser. It will fail if there is 0 or more than one node
        matched by [node]. *)

  val opt : 'a node -> 'a option t
    (** same as [one] but do not fail if there is no node matched by
        [node]. *)

  val any : 'a node -> 'a list t
    (** [any node] Parse all element matched by [node]. The resulting
        list is in the same order as the order in which nodes appears
        in the xml. *)
end

module Make(Xml : XML) : S with type xml = Xml.t
