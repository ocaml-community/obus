(*
 * caml.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** This module generate the caml code for marshaling (resp. loading)
    caml values to (resp. from) dbus values *)

open Camlp4.PreCast

type expr = Ast.expr

(** {6 Caml types} *)

type var = [ `Var of string ]
type 'a typ = [ `RTerm of string * 'a list ]
type mono = mono typ
type poly = [ poly typ | var ]

val v : string -> [> var ]
val int : [> 'a typ ]
val int32 : [> 'a typ ]
val int64 : [> 'a typ ]
val float : [> 'a typ ]
val bool : [> 'a typ ]
val char : [> 'a typ ]
val string : [> 'a typ ]
val list : 'a -> [> 'a typ ]
val array : 'a -> [> 'a typ ]
val dbus_value : [> 'a typ ]
val dbus_type : [> 'a typ ]
val tuple : ([> 'a typ ] as 'a) list -> 'a

val t0 : string -> [> 'a typ ]
val t1 : string -> 'a -> [> 'a typ ]
val t2 : string -> 'a -> 'a -> [> 'a typ ]
val t3 : string -> 'a -> 'a -> 'a -> [> 'a typ ]

(** {6 convertion} *)

type convertion_rule

val rule_dict : poly -> poly -> poly -> expr -> expr -> expr -> convertion_rule
val rule_array : poly -> poly -> expr -> expr -> expr -> convertion_rule
val rule_record : poly -> (string * poly) list -> convertion_rule
val rule_variant : poly -> (int * string * DBus.typ * mono) list -> convertion_rule
val rule_any : poly -> poly -> expr -> expr -> convertion_rule
val rule_map : poly -> string -> convertion_rule

val default_rules : convertion_rule list

(** {6 Code generation} *)

exception Cannot_generate

val generate_reader : convertion_rule list -> DBus.typ -> mono -> expr
val generate_writer : convertion_rule list -> mono -> DBus.typ -> expr

(** {6 Caml modules} *)

val default_type : DBus.typ -> mono

val write_modules : string -> mono Lmap.t -> unit

val args_mapper : (Arg.key * Arg.spec * Arg.doc) list
val args_generator : (Arg.key * Arg.spec * Arg.doc) list

val correct_module_name : string -> string
val correct_signal_name : string -> string
val correct_method_name : string -> string
val correct_arg_name : string -> string

val string_of_type : mono -> string
val type_of_string : string -> mono

val name : string
