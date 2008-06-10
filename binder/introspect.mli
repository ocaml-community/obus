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
  | Exception of doc * dbus_name * caml_name
      (** Declaration of a DBus exception *)
  | Convert of caml_type * caml_type * string option * string option
      (** [Convert(new_type, old_type, new_of_old, old_of_new)] add a
          rule for converting between this two types *)
  | Interf of string * bool
      (** Code to include in the interface *)
  | Implem of string
      (** Code to include in the implementation *)

type content = declaration list
    (** content of an interface *)

type interface = dbus_name * caml_name list * content * caml_type * caml_name option

type tree = (dbus_name * content * caml_type * caml_name option, caml_name) Tree.t
