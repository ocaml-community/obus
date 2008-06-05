(*
 * introspect.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Describe a DBus introspection file, plus some annotations for
    generating ocamls types and convertion functions *)

open Camlp4.PreCast
open Types

type caml_name = string
type dbus_name = string
type doc = string list

type argument = dbus_name option * caml_name option * dtype * doc
    (** Description of a method/signal argument, with

        - its dbus name
        - its caml name
        - its dbus type *)

type arguments = argument list * caml_type list
    (** A list of arguments, with their dbus informations and
        description of *)

type access = Read | Write | Read_write
    (** Access mode of a property *)

type mode =
    (** Mode for a flag/sum type *)
  | M_poly
      (** Polymorphic variant *)
  | M_variant
      (** Traditionnal variant *)
  | M_record
      (** For a flag, a record of bool, for a sum, a record of
          option *)

type proxy_type =
  | P_bus
  | P_connection

type declaration =
    (** A declaration inside a interface definition *)
  | Doc of doc
      (** Documentation *)
  | Method of doc * dbus_name * caml_name * arguments * arguments
      (** [Method(dbus_name, caml_name, in_args, out_args)] *)
  | Signal of doc * dbus_name * caml_name * arguments
      (** [Signal(dbus_name, caml_name, args)] *)
  | Property of doc * dbus_name * caml_name * dtype * caml_type * access
      (** [Property(dbus_name, caml_name, dbus_type, caml_type, access)] *)
  | Proxy of doc * caml_name * proxy_type * string option * string option
      (** [Proxy(caml_name, typ, destination, path)] proxy creation helper
          function *)
  | Flag of doc * caml_name * mode *  (int * caml_name * doc) list * bool * string option
      (** [Flag(name, mode, values, bitwise, external)] a flag type,
          used for functions which takes a list of flags or return a
          flag.

          Rules and generated type will depend on [mode].

          If [bitwise] is [true] then key have to be interpretted has
          bit position instead of value. *)
  | Convert of doc * caml_type * caml_type * string option * string option
      (** [Convert(type_a, type_b, a_of_b, b_of_a)] add a rule for
          converting between this two types *)
  | Variant of doc * caml_name * mode * (int * caml_name * dtype * caml_type list * doc) * string option
      (** [Variant(doc, name, mode, constructors, modul)] *)

type content = declaration list
    (** content of an interface *)

type interface = dbus_name * caml_name list * content

type tree = (dbus_name * content, caml_name) Tree.t
