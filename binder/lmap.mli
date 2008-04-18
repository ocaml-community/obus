(*
 * lmap.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

(** This module define a data structure used to represent the mapping
    of types/names of dbus types/names to destination languages
    types/names *)

type name = string
type name_mapping = name * name
    (** A mapping from a dbus name to a language native name *)

type 'a param_mapping =
    (** A mapping from a parameter with name and type *)
  | Arg of name_mapping * DBus.typ * 'a
      (** Single param mapping *)
  | Pack of (name_mapping * DBus.typ * 'a) list * 'a
      (** Mapping from a list of dbus parameters to one single
          parameters *)

type 'a def_mapping =
    (** A definition mapping *)
  | Method of name_mapping * 'a param_mapping list * 'a param_mapping list
  | Signal of name_mapping * 'a param_mapping list

type 'a tree =
    (** A mapping for a hierarchy of interfaces *)
  | Node of (name_mapping * 'a def_mapping list * 'a tree) list

type 'a t =
    (** A mapping with description (language name) *)
  | Mapping of string * 'a tree

val from_xml : (string -> 'a) -> Xml.xml -> 'a t
  (** [from_xml type_reader xml] Create a mapping from its xml
      representation *)

val to_xml : ('a -> string) -> 'a t -> Xml.xml
  (** [to_xml type_writer xml] Create an xml representation of a
      mapping *)

module Make(L : Binder.LanguageType) : sig
  val from_interfaces : 'a Interface.t Tree.t -> 'a t
    (** [from_interfaces interfaces] convert a interfaces hierarchy
        into a mapping for the specified language *)
end
