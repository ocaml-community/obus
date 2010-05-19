(*
 * utils.mli
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Utilities for tools *)

module IFSet : Set.S with type elt = OBus_introspect.interface
  (** Set of interfaces *)

val parse_xml : string -> IFSet.t
  (** [parse_xml file_name] parses [file_name] as an XML introspection
      file *)

val parse_idl : string -> IFSet.t
  (** [parse_xml file_name] parses [file_name] as an obus IDL file *)

val parse_file : string -> IFSet.t
  (** [parse_file file_name] parses [file_name] as an XML
      introspection file or as an IDL file (according to the file name
      extension), and returns the set of interfaces it contains. *)

val file_name_of_interface_name : OBus_name.interface -> string
  (** Convert an interface name into a valid module file name *)

val convertor_send : OBus_value.T.single -> string option
  (** [convertor_send typ] returns an expression which convert caml
      values before they are sent. It returns [None] if no conversion
      is needed. *)

val convertor_recv : OBus_value.T.single -> string option
  (** [convertor_recv typ] returns an expression which convert caml
      values after they are received. It returns [None] if no
      conversion is needed. *)

val make_annotation : OBus_introspect.name -> string
  (** [make_annotation name] returns the code for the given annotation *)
