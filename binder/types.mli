(*
 * types.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type mono
  (** mono is an empty type *)
type poly

type (+'t, +'var) term =
    private
  | Type of 't * ('t, 'var) term list
  | Cons of ('t, 'var) term * ('t, 'var) term
  | Nil
  | Var of 'var

type 'a typ = ('a, mono) term
type 'a pattern = ('a, poly) term

val fresh : unit -> ('a, poly) term
  (** [fresh ()] create a fresh new variable *)

val v : string -> ('a, poly) term
  (** [v x] create a variable *)

val typ : 'a -> ('a, 'b) term list -> ('a, 'b) term
  (** [typ id args] create a new type *)

val cons : ('a, 'b) term -> ('a, 'b) term -> ('a, 'b) term
  (** [cons x y] create a cons containing [x] and [y]. [y] must be a
      variable of [nil] *)
val nil : ('a, 'b) term

val tuple : ('a, 'b) term list -> ('a, 'b) term
  (** [tuple l] create a tuple from a list of types *)

val list_of_tuple : ('a, 'b) term -> ('a, 'b) term list
  (** [list_of_tuple tup] return the list of types contained in
      [tup]. *)

type ident = Camlp4.PreCast.Ast.ident
type expr = Camlp4.PreCast.Ast.expr
type patt = Camlp4.PreCast.Ast.patt

type caml_id = string
type 'a caml_type = (caml_id, 'a) term

val int : 'a caml_type
val int32 : 'a caml_type
val int64 : 'a caml_type
val float : 'a caml_type
val bool : 'a caml_type
val char : 'a caml_type
val string : 'a caml_type
val list : 'a caml_type -> 'a caml_type
val array : 'a caml_type -> 'a caml_type
val dbus_value : 'a caml_type
val dbus_types : 'a caml_type

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
type dtypes = dtype list

type dbus_id
type 'a dbus_type = (dbus_id, 'a) term

val dbyte : 'a dbus_type
val dboolean : 'a dbus_type
val dint16 : 'a dbus_type
val dint32 : 'a dbus_type
val dint64 : 'a dbus_type
val duint16 : 'a dbus_type
val duint32 : 'a dbus_type
val duint64 : 'a dbus_type
val ddouble : 'a dbus_type
val dstring : 'a dbus_type
val dsignature : 'a dbus_type
val dobject_path : 'a dbus_type
val darray : 'a dbus_type -> 'a dbus_type
val ddict : 'a dbus_type -> 'a dbus_type -> 'a dbus_type
val dstructure : 'a dbus_type -> 'a dbus_type
val dvariant : 'a dbus_type

val dtypes_of_signature : string -> dtypes
val signature_of_dtypes : dtypes -> string

val dbus_type_of_dtypes : dtypes -> 'a dbus_type
