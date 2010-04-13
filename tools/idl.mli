(*
 * idl.mli
 * -------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Intermediate language to write D-Bus interface *)

exception Parse_failure of Lexing.position * string
  (** Errors raised on parsing failure *)

val parse : string -> OBus_introspect.interface list
  (** [parse lexbuf] parses all interfaces defined in the file
      [file_name] *)

val print : string -> OBus_introspect.interface list -> unit
  (** [print file_name interfaces] writes to [file_name] the given
      interfaces in the obus idl format *)

