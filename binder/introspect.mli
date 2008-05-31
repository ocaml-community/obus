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

open Types

type name = string
type doc = string list

type argument = name option * name option * dbus_type * doc
    (** Description of a method/signal argument, with

        - its dbus name
        - its caml name
        - its dbus type *)

type arguments = argument list * caml_type
    (** A list of arguments, with their dbus informations and
        description of *)

type access = Read | Write | Read_write
    (** Access mode of a property *)

type flag_mode =
  | F_poly
  | F_variant
  | F_record

type proxy_type =
  | P_bus
  | P_connection

type declaration =
    (** A declaration inside a interface definition *)
  | Doc of doc
      (** Documentation *)
  | Method of doc * name * name * arguments * arguments
      (** [Method(dbus_name, caml_name, in_args, out_args)] *)
  | Signal of doc * name * name * arguments
      (** [Signal(dbus_name, caml_name, args)] *)
  | Property of doc * name * name * dbus_type * access
      (** [Property(dbus_name, caml_name, dbus_type, access)] *)
  | Proxy of doc * name * proxy_type * string option * string option
      (** [Proxy(caml_name, typ, destination, path)] proxy creation helper
          function *)
  | Flag of doc * name * flag_mode *  (int * name * doc) list * bool * string option
      (** [Flag(name, mode, values, bitwise, modul)] a flag type, used for
          functions which takes a list of flags or return a flag.

          Rules and generated type will depend on [typ].

          If [bitwise] is [true] then key have to be interpretted has
          bit position instead of value. *)
  | Convert of doc * caml_type * caml_type * string option * string option
      (** [Convert(type_a, type_b, a_of_b, b_of_a)] add a rule for
          converting between this two types *)

type module_tree = Node of name * declaration list * (name * module_tree) list
  (** [Node(dbus_interface_name, content, sons) A hierarchy of ocaml
      modules for DBus interfaces *)

val parse_files : string list -> module_tree
  (** [parse_files files] construct a module hierarchy from xml files *)

val contain_dbus_declaration : declaration list -> bool
  (** [contain_dbus_declaration decls] return [true] if [decls]
      containt at leat one of [Method], [Signal] or [Property] *)
