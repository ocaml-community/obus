(*
 * oBus_idl.mli
 * ------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Intermediate language for writing D-Bus interfaces *)

exception Parse_failure of string
  (** Exception raised when parsing fails for some reason. The
      argument is an error message. *)

val parse : ?file_name : string -> char Stream.t -> OBus_introspect_ext.interface list
  (** [parse stream] parses the given stream. [file_name] is used for
      error messages. *)

val parse_file : string -> OBus_introspect_ext.interface list
  (** Helper to parse the contents of a file *)

val print : Format.formatter -> OBus_introspect_ext.interface list -> unit
  (** [print pp interfaces] prints the given interfaces on [pp] in the
      obus idl format *)

val print_file : string -> OBus_introspect_ext.interface list -> unit
  (** Helper to print to a file *)
