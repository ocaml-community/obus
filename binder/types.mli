(*
 * types.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type var = string

type typ =
  | Type of string * typ list
  | Cons of typ * typ
  | Nil
  | Var of var

val v : string -> typ
val typ : string -> typ list -> typ
val cons : typ -> typ -> typ
val nil : typ
val tuple : typ list -> typ
  (** Fonctionnal version of typ constructors *)

val list_of_tuple : typ -> typ list
  (** [list_of_tuple tup] return the list of types contained in
      [tup]. Return a list containing only [tup] if it is not a
      tuple. *)

val string_of_type : string -> typ -> string
  (** [string_of_type empty typ] return a string representation of the
      type. [empty] is the string representation of the empty
      tuple. *)

val ctyp_of_type : typ -> Camlp4.PreCast.Ast.ctyp
  (** [ctyp_of_type typ] return a camlp4 ast representation op
      [typ] *)

val ctyp_func_of_types : typ list -> Camlp4.PreCast.Ast.ctyp -> Camlp4.PreCast.Ast.ctyp
  (** [ctyp_func_of_types types acc] return the camlp4 representation
      of the type [t1 -> t2 -> ... -> tn -> acc] where [types] = [t1;
      t2; ...; tn] *)

val type_of_string : string -> string -> typ
  (** [type_of_string str empty] parse a string representation of a
      string *)

type ident = Camlp4.PreCast.Ast.ident
type expr = Camlp4.PreCast.Ast.expr
type patt = Camlp4.PreCast.Ast.patt

type caml_type = typ

val unit : caml_type
val int : caml_type
val int32 : caml_type
val int64 : caml_type
val float : caml_type
val bool : caml_type
val char : caml_type
val string : caml_type
val list : caml_type -> caml_type
val array : caml_type -> caml_type
val obus_value : caml_type
val obus_values : caml_type
val obus_dtype : caml_type
val obus_dtypes : caml_type

type dtype =
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
  | Tarray of dtype
  | Tdict of dtype * dtype
  | Tstructure of dtype list
  | Tvariant

type dbus_type = dtype

val dbus_type_of_signature : string -> dbus_type
val signature_of_dbus_type : dbus_type -> string
val dbus_types_of_signature : string -> dbus_type list
val signature_of_dbus_types : dbus_type list -> string
