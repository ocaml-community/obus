(*
 * oBus_introspect.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Introspection *)

type node = string
type document = OBus_interface.t list * node list
val tdocument : document OBus_type.ty_basic

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

(** {6 Service introspection} *)

val introspect : OBus_connection.t -> ?service:OBus_name.connection -> OBus_path.t -> document Lwt.t
  (** [introspect connection ?service path] introspect the object with
      path [path] on the given service *)
