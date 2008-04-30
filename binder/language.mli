(*
 * language.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type S = sig
  val name : string
    (** The language name *)

  type id
    (** Identifier for types *)

  type typ = id Type.typ
      (** Language types *)

  val correct_module_name : string -> string
  val correct_signal_name : string -> string
  val correct_method_name : string -> string
  val correct_arg_name : string -> string
    (** Convert a dbus identifier into a valid identifier of the
        destination language *)

  val default_type : DBus.dtypes -> typ
    (** [default typ] The default type corresponding to a dbus type *)

  val string_of_type : typ -> string
  val type_of_string : string -> typ
    (** Type reading/writing *)

  val generate : string -> typ Lmap.t -> unit
    (** [generate name mapping] generate and write the modules code
        and signature to appropriate files *)

  val args_mapper : (Arg.key * Arg.spec * Arg.doc) list
  val args_generator : (Arg.key * Arg.spec * Arg.doc) list
    (** Additionnal arguments for the program *)
end
