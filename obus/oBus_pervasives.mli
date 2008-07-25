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

open OBus_annot

type toto = [ `ok | `not_ok ]
    with_dbus: dtoto
type dtoto
val dtoto : dtoto

val ob_byte : (char, _, dbyte) OBus_comb.one
val ob_char : (char, _, dbyte) OBus_comb.one
val ob_boolean : (bool, _, dboolean) OBus_comb.one
val ob_bool : (bool, _, dboolean) OBus_comb.one
val ob_int8 : (int, _, dint8) OBus_comb.one
val ob_uint8 : (int, _, duint8) OBus_comb.one
val ob_int16 : (int, _, dint16) OBus_comb.one
val ob_uint16 : (int, _, duint16) OBus_comb.one
val ob_int32 : (int32, _, dint32) OBus_comb.one
val ob_uint32 : (int32, _, duint32) OBus_comb.one
val ob_int64 : (int64, _, dint64) OBus_comb.one
val ob_uint64 : (int64, _, duint64) OBus_comb.one
val ob_int : (int, _, dint32) OBus_comb.one
val ob_uint : (int, _, duint32) OBus_comb.one
val ob_double : (float, _, ddouble) OBus_comb.one
val ob_float : (float, _, ddouble) OBus_comb.one
val ob_signature : (signature, _, dsignature) OBus_comb.one
val ob_object_path : (OBus_path.t, _, dobject_path) OBus_comb.one
val ob_path : (OBus_path.t, _, dobject_path) OBus_comb.one
val ob_list : ('a, 'da) OBus_comb.single_p -> ('a list, _, 'da darray) OBus_comb.one
val ob_set : ('a, 'da) OBus_comb.single_p -> ('a list, _, 'da darray) OBus_comb.one
  (** The difference between [ob_set] and [ob_list] is that [ob_set]
      is more efficient than [ob_list] but does not keep element
      order *)
val ob_assoc : ('a, 'da) OBus_comb.basic_p -> ('b, 'db) OBus_comb.single_p -> (('a * 'b) list, _, ('da, 'db) ddict) OBus_comb.one
val ob_byte_array : (string, _, dbyte_array) OBus_comb.one
  (** Map a DBus array of bytes into a string. *)
val ob_structure : ('a, 'da) OBus_comb.sequence_p -> ('a, _, 'da dstruct) OBus_comb.one
  (** Pack a sequence of values into one *)
val ob_variant : (OBus_value.single, _, dvariant) OBus_comb.one
val ob_unit : (unit, 'a, 'a) OBus_comb.t
val ob_pair : ('a1, 'b1, 'b2) OBus_comb.t -> ('a2, 'b2, 'b3) OBus_comb.t -> ('a1 * 'a2, 'b1, 'b3) OBus_comb.t

val ob_return : ('a, unit, 'b) OBus_comb.t -> ('c, 'a, 'c) OBus_comb.func
  (** Same as [OBus_conv.return] *)

val (-->) : ('a, unit, 'da) OBus_comb.t -> ('b, 'c, 'd) OBus_comb.func -> ('a -> 'b, 'c, 'd) OBus_comb.func
  (** Same as [OBus_conv.abstract] *)

val ob_tuple2 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a1 * 'a2, 'b1, 'b3) OBus_comb.t
val ob_tuple3 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a3, 'b3, 'b4) OBus_comb.t ->
  ('a1 * 'a2 * 'a3, 'b1, 'b4) OBus_comb.t
val ob_tuple4 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a3, 'b3, 'b4) OBus_comb.t ->
  ('a4, 'b4, 'b5) OBus_comb.t ->
  ('a1 * 'a2 * 'a3 * 'a4, 'b1, 'b5) OBus_comb.t
val ob_tuple5 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a3, 'b3, 'b4) OBus_comb.t ->
  ('a4, 'b4, 'b5) OBus_comb.t ->
  ('a5, 'b5, 'b6) OBus_comb.t ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5, 'b1, 'b6) OBus_comb.t
val ob_tuple6 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a3, 'b3, 'b4) OBus_comb.t ->
  ('a4, 'b4, 'b5) OBus_comb.t ->
  ('a5, 'b5, 'b6) OBus_comb.t ->
  ('a6, 'b6, 'b7) OBus_comb.t ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6, 'b1, 'b7) OBus_comb.t
val ob_tuple7 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a3, 'b3, 'b4) OBus_comb.t ->
  ('a4, 'b4, 'b5) OBus_comb.t ->
  ('a5, 'b5, 'b6) OBus_comb.t ->
  ('a6, 'b6, 'b7) OBus_comb.t ->
  ('a7, 'b7, 'b8) OBus_comb.t ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7, 'b1, 'b8) OBus_comb.t
val ob_tuple8 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a3, 'b3, 'b4) OBus_comb.t ->
  ('a4, 'b4, 'b5) OBus_comb.t ->
  ('a5, 'b5, 'b6) OBus_comb.t ->
  ('a6, 'b6, 'b7) OBus_comb.t ->
  ('a7, 'b7, 'b8) OBus_comb.t ->
  ('a8, 'b8, 'b9) OBus_comb.t ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8, 'b1, 'b9) OBus_comb.t
val ob_tuple9 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a3, 'b3, 'b4) OBus_comb.t ->
  ('a4, 'b4, 'b5) OBus_comb.t ->
  ('a5, 'b5, 'b6) OBus_comb.t ->
  ('a6, 'b6, 'b7) OBus_comb.t ->
  ('a7, 'b7, 'b8) OBus_comb.t ->
  ('a8, 'b8, 'b9) OBus_comb.t ->
  ('a9, 'b8, 'b10) OBus_comb.t ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9, 'b1, 'b10) OBus_comb.t
val ob_tuple10 :
  ('a1, 'b1, 'b2) OBus_comb.t ->
  ('a2, 'b2, 'b3) OBus_comb.t ->
  ('a3, 'b3, 'b4) OBus_comb.t ->
  ('a4, 'b4, 'b5) OBus_comb.t ->
  ('a5, 'b5, 'b6) OBus_comb.t ->
  ('a6, 'b6, 'b7) OBus_comb.t ->
  ('a7, 'b7, 'b8) OBus_comb.t ->
  ('a8, 'b8, 'b9) OBus_comb.t ->
  ('a9, 'b8, 'b10) OBus_comb.t ->
  ('a10, 'b8, 'b11) OBus_comb.t ->
  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10, 'b1, 'b11) OBus_comb.t
