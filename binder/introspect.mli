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

type argument = name option * name option * dbus_type
    (** Description of a method/signal argument, with

        - its dbus name
        - its caml name
        - its dbus type *)

type arguments = argument list * caml_type
    (** A list of arguments, with their dbus informations and
        description of *)

type access = Read | Write | Read_write
    (** Access mode of a property *)

type declaration =
    (** A declaration inside a interface definition *)
  | Method of name * name * arguments * arguments
      (** [Method(dbus_name, caml_name, in_args, out_args)] *)
  | Signal of name * name * arguments
      (** [Signal(dbus_name, caml_name, args)] *)
  | Property of name * name * dbus_type * access
      (** [Property(dbus_name, caml_name, dbus_type, access)] *)
  | Proxy of name * [ `Bus | `Connection ] * string option * string option
      (** [Proxy(caml_name, typ, destination, path)] proxy creation helper
          function *)
  | Flag of name * (int * name) list

type module_tree = Node of name * declaration list * (name * module_tree) list
  (** [Node(dbus_interface_name, content, sons) A hierarchy of ocaml
      modules for DBus interfaces *)

val parse_files : string list -> module_tree
  (** [parse_files files] construct a module hierarchy from xml files *)

val contain_dbus_declaration : declaration list -> bool
  (** [contain_dbus_declaration decls] return [true] if [decls]
      containt at leat one of [Method], [Signal] or [Property] *)
