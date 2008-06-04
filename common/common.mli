(*
 * common.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Shared code *)

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

val signature_of_dtype : dtype -> string
val signature_of_dtypes : dtypes -> string
  (** Calculate the dbus signature of one or more dbus types (assume
      that the dbus type is weel-formed, i.e. the key type argument of
      a dict is always a basic type) *)

val dtype_of_signature : string -> dtype
val dtypes_of_signature : string -> dtypes
  (** Parse a dbus signature *)

val dtype_signature_size : dtype -> int
val dtypes_signature_size : dtypes -> int
  (** Compute the size that a signature will take *)

val unsafe_read_dtype : string -> int -> int * dtype
val unsafe_read_dtypes : string -> int -> int * dtypes
  (** Read a signature containing one or more dbus types followed by a
      null character. The two functions assumes that the null
      character is present before the end of the buffer. *)

val unsafe_write_dtype : string -> int -> dtype -> int
val unsafe_write_dtypes : string -> int -> dtypes -> int
  (** Write a signature. The two function assumes that there is enough
      space in the buffer to write the entire signature *)
