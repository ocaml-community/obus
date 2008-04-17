(*
 * dBus.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type typ =
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
  | Tarray of typ
  | Tdict of typ * typ
  | Tstruct of typ list
  | Tvariant

val type_of_string : string -> typ
  (** [type_of_string str] Convert a dbus type representation with
      type codes into the caml represention *)

val string_of_type : typ -> string
  (** [string_of_type typ] Inverse of type_from_string *)

