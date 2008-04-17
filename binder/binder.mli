(*
 * binder.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

module type LanguageType =
sig
  val name : string

  type mono
  type poly
    (** Represention of the language type *)

  type expr
  type module_sig = mono Interface.t
  type module_str

  val correct_module_name : string -> string
  val correct_signal_name : string -> string
  val correct_method_name : string -> string

  val default_type : DBus.typ -> mono
    (** [default typ] The default type for a dbus type *)

  val string_of_type : mono -> string
  val type_of_string : string -> mono

  val write_module_sigs : string -> module_sig Tree.t -> unit
  val write_module_strs : string -> module_str Tree.t -> unit

  val args : (Arg.key * Arg.spec * Arg.doc) list
end

module MakeSig(L : LanguageType) :
sig
  val main : unit -> unit
end

module MakeStr(L : LanguageType) :
sig
  val main : unit -> unit
end

