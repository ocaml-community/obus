(*
 * oBus_pervasives.mli
 * -------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Standart types *)

(** This modules define combinators for standard caml types. It is
    automatically opened by the syntax extension. *)

open OBus_conv
open OBus_wire

val ob_byte : (char, _, dbyte) one
val ob_char : (char, _, dbyte) one
val ob_boolean : (bool, _, dboolean) one
val ob_bool : (bool, _, dboolean) one
val ob_int8 : (int, _, dbyte) one
val ob_uint8 : (int, _, dbyte) one
val ob_int16 : (int, _, dint16) one
val ob_uint16 : (int, _, duint16) one
val ob_int : (int, _, dint32) one
val ob_uint : (int, _, duint32) one
val ob_int64 : (int64, _, dint64) one
val ob_uint64 : (int64, _, duint64) one
val ob_double : (float, _, ddouble) one
val ob_float : (float, _, ddouble) one
val ob_signature : (OBus_types.signature, _, dsignature) one
val ob_object_path : (OBus_path.t, _, dobject_path) one
val ob_path : (OBus_path.t, _, dobject_path) one
val ob_list : ('a, unit, 'da) one -> ('a list, _, 'da darray) one
val ob_set : ('a, unit, 'da) one -> ('a list, _, 'da darray) one
  (** The difference between [ob_set] and [ob_list] is that [ob_set]
      is more efficient than [ob_list] but does not keep element
      order *)
val ob_assoc : ('a, unit, 'da) one -> ('b, unit, 'db) one -> (('a * 'b) list, _, ('da, 'db) ddict) one
val ob_byte_array : (string, _, dbyte darray) one
  (** Map a DBus array of bytes into a string. *)
val ob_structure : ('a, unit, 'da) simple -> ('a, _, 'da dstruct) one
  (** Pack a sequence of values into one *)
val ob_variant : (OBus_value.single, _, dvariant) one
val ob_unit : (unit, 'a, 'a) simple
val ob_pair : ('a1, 'b1, 'b2) simple -> ('a2, 'b2, 'b3) simple -> ('a1 * 'a2, 'b1, 'b3) simple

val ob_return : ('a, unit, 'b) simple -> ('c, 'a, 'c) func
  (** Same as [OBus_conv.return] *)

val (-->) : ('a, unit, 'da) simple -> ('b, 'c, 'd) func -> ('a -> 'b, 'c, 'd) func
  (** Same as [OBus_conv.abstract] *)

val ob_tuple2 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a1 * 'a2, 'b1, 'b3) simple
val ob_tuple3 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a3, 'b3, 'b4) simple ->
  ('a1 * 'a2 * 'a3, 'b1, 'b4) simple
val ob_tuple4 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a3, 'b3, 'b4) simple ->
  ('a4, 'b4, 'b5) simple ->
  ('a1 * 'a2 * 'a3 * 'a4, 'b1, 'b5) simple
val ob_tuple5 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a3, 'b3, 'b4) simple ->
  ('a4, 'b4, 'b5) simple ->
  ('a5, 'b5, 'b6) simple ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5, 'b1, 'b6) simple
val ob_tuple6 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a3, 'b3, 'b4) simple ->
  ('a4, 'b4, 'b5) simple ->
  ('a5, 'b5, 'b6) simple ->
  ('a6, 'b6, 'b7) simple ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6, 'b1, 'b7) simple
val ob_tuple7 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a3, 'b3, 'b4) simple ->
  ('a4, 'b4, 'b5) simple ->
  ('a5, 'b5, 'b6) simple ->
  ('a6, 'b6, 'b7) simple ->
  ('a7, 'b7, 'b8) simple ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7, 'b1, 'b8) simple
val ob_tuple8 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a3, 'b3, 'b4) simple ->
  ('a4, 'b4, 'b5) simple ->
  ('a5, 'b5, 'b6) simple ->
  ('a6, 'b6, 'b7) simple ->
  ('a7, 'b7, 'b8) simple ->
  ('a8, 'b8, 'b9) simple ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8, 'b1, 'b9) simple
val ob_tuple9 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a3, 'b3, 'b4) simple ->
  ('a4, 'b4, 'b5) simple ->
  ('a5, 'b5, 'b6) simple ->
  ('a6, 'b6, 'b7) simple ->
  ('a7, 'b7, 'b8) simple ->
  ('a8, 'b8, 'b9) simple ->
  ('a9, 'b8, 'b10) simple ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9, 'b1, 'b10) simple
val ob_tuple10 :
  ('a1, 'b1, 'b2) simple ->
  ('a2, 'b2, 'b3) simple ->
  ('a3, 'b3, 'b4) simple ->
  ('a4, 'b4, 'b5) simple ->
  ('a5, 'b5, 'b6) simple ->
  ('a6, 'b6, 'b7) simple ->
  ('a7, 'b7, 'b8) simple ->
  ('a8, 'b8, 'b9) simple ->
  ('a9, 'b8, 'b10) simple ->
  ('a10, 'b8, 'b11) simple ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10, 'b1, 'b11) simple
