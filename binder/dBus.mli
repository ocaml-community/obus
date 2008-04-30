(*
 * dBus.mli
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Type

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
type 'a dbus_type = (dbus_id, 'a) Type.term

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

type interface = dtypes Sig.t

val from_xml : Xml.xml -> interface list
  (** [from_xml xml] parse a xml obtained from introspection into a
      list of interfaces *)
