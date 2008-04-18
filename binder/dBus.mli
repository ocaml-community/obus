(*
 * dBus.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type typ =
  | Tbyte
  | Tboolean
  | Tint16
  | Tint32
  | Tint64
  | Tuint16
  | Tuint32
  | Tuint64
  | Tdouble
  | Tstring
  | Tsignature
  | Tobject_path
  | Tarray of typ
  | Tdict of typ * typ
  | Tstruct of typ list
  | Tvariant

val type_of_string : string -> typ
  (** [type_of_string str] Convert a dbus type representation with
      type codes into the caml represention *)

val string_of_type : typ -> string
  (** [string_of_type typ] Inverse of type_from_string *)

type name = string

type param = Arg of name * typ
  (** A parameter with its name and type in a definition *)

type def =
    (** A definition in an interface *)
  | Method of name * param list * param list
  | Signal of name * param list

type tree =
    (** A hierarchy of interfaces *)
  | Node of (name * def list * tree) list

val add : name -> def list -> tree -> tree
  (** [add name defs tree] add an interface into a tree *)

val from_xml : Xml.xml -> (name * def list) list
  (** [from_xml xml] parse a xml obtained from introspection into a
      list of interfaces with their names *)
