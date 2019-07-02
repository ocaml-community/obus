(*
 * oBus_introspect.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** D-Bus obejct introspection *)

type name = string

type annotation = name * string
type argument = name option * OBus_value.T.single

type access = Read | Write | Read_write
    (** Access mode of properties *)

type member =
  | Method of name * argument list * argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * OBus_value.T.single * access * annotation list

type interface = name * member list * annotation list
type node = OBus_path.element

type document = interface list * node list

(** {6 Xml conversion} *)

exception Parse_failure of Xmlm.pos * string

val input : Xmlm.input -> document
  (** Try to read an xml document as an introspection document.

      @raise Parse_failure if the parsing fail. *)

val output : Xmlm.output -> document -> unit
  (** Create an xml from an introspection document *)

(** {6 Well-known annotations} *)

val deprecated : name
  (** The [org.freedesktop.DBus.Deprecated] annotation *)

val csymbol : name
  (** The [org.freedesktop.DBus.GLib.CSymbol] annotation *)

val no_reply : name
  (** The [org.freedesktop.DBus.Method.NoReply] annotation *)

val emits_changed_signal : name
  (** The [org.freedesktop.DBus.Property.EmitsChangedSignal] annotation *)
