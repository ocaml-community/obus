(*
 * dBus.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

type dbus_type =
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
  | Tarray of dbus_type
  | Tdict of dbus_type * dbus_type
  | Tstruct of dbus_type list
  | Tvariant
