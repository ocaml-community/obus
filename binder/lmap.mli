(*
 * lmap.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Description of a mapping between DBus types/names to the
    destination language types/names *)

type 'a t = {
  language : string;
  sigs : 'a Sig.tree;
  map : (string * DBus.interface) list;
}

val empty : string -> 'a t
  (** [empty] empty mapping *)

val from_xml : (string -> 'a) -> Xml.xml -> 'a t
  (** [from_xml type_reader xml] Create a mapping from its xml
      representation *)

val to_xml : ('a -> string) -> 'a t -> Xml.xml
  (** [to_xml type_writer xml] Create an xml representation of a
      mapping *)

val merge : 'a t -> 'a t -> 'a t
  (** [merge a b] merge two mapping *)
