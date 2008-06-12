(*
 * types.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type var = string

type caml_type =
    (** The subset of caml types that we handle *)
  | Type of string * caml_type list
  | Tuple of caml_type list
  | Var of var

val v : string -> caml_type
val typ : string -> caml_type list -> caml_type
val tuple : caml_type list -> caml_type
  (** Fonctionnal version of typ constructors *)

val unit : caml_type
val int : caml_type
val int32 : caml_type
val int64 : caml_type
val float : caml_type
val bool : caml_type
val char : caml_type
val string : caml_type
val path : caml_type
val list : caml_type -> caml_type
  (** Standart caml types *)
val obus_value : caml_type
val obus_values : caml_type
val obus_dtype : caml_type
val obus_dtypes : caml_type
  (** Types specific to obus *)

val list_of_tuple : caml_type -> caml_type list
  (** [list_of_tuple tup] return the list of types contained in
      [tup]. Return a list containing only [tup] if it is not a
      tuple. *)

val string_of_caml_type : caml_type -> string
  (** [string_of_type typ] return a string representation of
      [typ] *)

val ctyp_of_caml_type : caml_type -> Camlp4.PreCast.Ast.ctyp
  (** [ctyp_of_type typ] return a camlp4 ast representation of
      [typ] *)

val ctyp_func_of_caml_types : caml_type list -> Camlp4.PreCast.Ast.ctyp -> Camlp4.PreCast.Ast.ctyp
  (** [ctyp_func_of_types types acc] return the camlp4 representation
      of the type [t1 -> t2 -> ... -> tn -> acc] where [types] = [t1;
      t2; ...; tn] *)

val caml_type_of_string : string -> caml_type
  (** [type_of_string str] parse a string representation of a type *)

type ident = Camlp4.PreCast.Ast.ident
type expr = Camlp4.PreCast.Ast.expr
type patt = Camlp4.PreCast.Ast.patt

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

val dtype_of_signature : string -> dtype

type dbus_type =
  | Tsingle of dtype
  | Tseq of dtype list

val dbus_type_of_signature : string -> dbus_type
val signature_of_dbus_type : dbus_type -> string
  (** Read/write a signature for dbus single type *)

val string_of_eqn : caml_type * dbus_type -> string
  (** [string_of_eqn eqn] return a string describing a
      [GenSerializer.eqn] equation *)
