(*
 * utils.mli
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Utilities for tools *)

module IFSet : Set.S with type elt = OBus_introspect_ext.interface
  (** Set of interfaces *)

val parse_xml : string -> IFSet.t
  (** [parse_xml file_name] parses [file_name] as an XML introspection
      file *)

val file_name_of_interface_name : OBus_name.interface -> string
  (** Convert an interface name into a valid module file name *)

val convertor_send : bool -> OBus_introspect_ext.term -> string option
  (** [convertor_send paren typ] returns an expression which convert
      caml values before they are sent. It returns [None] if no
      conversion is needed. If [paren] is [true] then no parenthesis
      will be used, otherwise the expression may be surrounded by
      parenthesis if needed *)

val convertor_recv : bool -> OBus_introspect_ext.term -> string option
  (** [convertor_recv paren typ] returns an expression which convert
      caml values after they are received. It returns [None] if no
      conversion is needed. *)

val make_annotation : OBus_introspect.name -> string
  (** [make_annotation name] returns the code for the given annotation *)
