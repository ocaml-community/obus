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

open OBus_types

(** {6 Simple combinators} *)

val ob_byte : (char, _, dbyte) OBus_comb.one
val ob_char : (char, _, dbyte) OBus_comb.one
val ob_boolean : (bool, _, dboolean) OBus_comb.one
val ob_bool : (bool, _, dboolean) OBus_comb.one
val ob_int8 : (int, _, dbyte) OBus_comb.one
val ob_uint8 : (int, _, dbyte) OBus_comb.one
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
val ob_string : (string, _, dstring) OBus_comb.one
val ob_signature : (OBus_types.signature, _, dsignature) OBus_comb.one
val ob_object_path : (OBus_path.t, _, dobject_path) OBus_comb.one
val ob_path : (OBus_path.t, _, dobject_path) OBus_comb.one
val ob_proxy : (OBus_proxy.t, _, dobject_path) OBus_comb.one
val ob_list : ('a, 'da) OBus_comb.single_p -> ('a list, _, 'da darray) OBus_comb.one
val ob_set : ('a, 'da) OBus_comb.single_p -> ('a list, _, 'da darray) OBus_comb.one
  (** The difference between [ob_set] and [ob_list] is that [ob_set]
      is more efficient than [ob_list] but does not keep element
      order *)
val ob_assoc : ('a, 'da) OBus_comb.basic_p -> ('b, 'db) OBus_comb.single_p -> (('a * 'b) list, _, ('da, 'db) ddict) OBus_comb.one
val ob_byte_array : (string, _, dbyte darray) OBus_comb.one
  (** DBus array of bytes seen as a string. *)
val ob_structure : ('a, 'da) OBus_comb.sequence_p -> ('a, _, 'da dstruct) OBus_comb.one
  (** Pack a sequence of values into one *)
val ob_variant : (OBus_value.single, _, dvariant) OBus_comb.one
val ob_unit : (unit, 'a, 'a) OBus_comb.t
val ob_pair : ('a, 'b, 'c) OBus_comb.t -> ('d, 'e, 'b) OBus_comb.t -> ('a * 'd, 'e, 'c) OBus_comb.t

(** {6 Context combinators} *)

(** These combinators let you to retreive some information about the
    context of a call. They do not modify the DBus signature. *)

val ob_sender : (string option, 'a, 'a) OBus_comb.t
  (** This combinator return the sender of a message *)

val ob_connection : (OBus_connection.t, 'a, 'a) OBus_comb.t
  (** This one return the connection from which a message was
      received *)

(** {6 Functionnal combinators} *)

val ob_reply : ('a, 'b) OBus_comb.sequence_p -> ('c, 'c, 'a, unit, 'b) OBus_comb.func
  (** Same as [OBus_comb.reply] *)

val (-->) : ('a, 'db, 'da) OBus_comb.t -> ('b, 'c, 'd, 'db, 'e) OBus_comb.func -> ('a -> 'b, 'c, 'd, 'da, 'e) OBus_comb.func
  (** Same as [OBus_comb.abstract] *)

(** {6 Tuples} *)

(** These combinators are used by the syntax extension, but if you do
    not use the syntax extension you can use them to construct tuple
    combinators *)

val ob_tuple2 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('a * 'd, 'e, 'c) OBus_comb.t
val ob_tuple3 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('f, 'g, 'e) OBus_comb.t ->
  ('a * 'd * 'f, 'g, 'c) OBus_comb.t
val ob_tuple4 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('f, 'g, 'e) OBus_comb.t ->
  ('h, 'i, 'g) OBus_comb.t ->
  ('a * 'd * 'f * 'h, 'i, 'c) OBus_comb.t
val ob_tuple5 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('f, 'g, 'e) OBus_comb.t ->
  ('h, 'i, 'g) OBus_comb.t ->
  ('j, 'k, 'i) OBus_comb.t ->
  ('a * 'd * 'f * 'h * 'j, 'k, 'c) OBus_comb.t
val ob_tuple6 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('f, 'g, 'e) OBus_comb.t ->
  ('h, 'i, 'g) OBus_comb.t ->
  ('j, 'k, 'i) OBus_comb.t ->
  ('l, 'm, 'k) OBus_comb.t ->
  ('a * 'd * 'f * 'h * 'j * 'l, 'm, 'c) OBus_comb.t
val ob_tuple7 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('f, 'g, 'e) OBus_comb.t ->
  ('h, 'i, 'g) OBus_comb.t ->
  ('j, 'k, 'i) OBus_comb.t ->
  ('l, 'm, 'k) OBus_comb.t ->
  ('n, 'o, 'm) OBus_comb.t ->
  ('a * 'd * 'f * 'h * 'j * 'l * 'n, 'o, 'c) OBus_comb.t
val ob_tuple8 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('f, 'g, 'e) OBus_comb.t ->
  ('h, 'i, 'g) OBus_comb.t ->
  ('j, 'k, 'i) OBus_comb.t ->
  ('l, 'm, 'k) OBus_comb.t ->
  ('n, 'o, 'm) OBus_comb.t ->
  ('p, 'q, 'o) OBus_comb.t ->
  ('a * 'd * 'f * 'h * 'j * 'l * 'n * 'p, 'q, 'c) OBus_comb.t
val ob_tuple9 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('f, 'g, 'e) OBus_comb.t ->
  ('h, 'i, 'g) OBus_comb.t ->
  ('j, 'k, 'i) OBus_comb.t ->
  ('l, 'm, 'k) OBus_comb.t ->
  ('n, 'o, 'm) OBus_comb.t ->
  ('p, 'q, 'o) OBus_comb.t ->
  ('r, 's, 'q) OBus_comb.t ->
  ('a * 'd * 'f * 'h * 'j * 'l * 'n * 'p * 'r, 's, 'c) OBus_comb.t
val ob_tuple10 :
  ('a, 'b, 'c) OBus_comb.t ->
  ('d, 'e, 'b) OBus_comb.t ->
  ('f, 'g, 'e) OBus_comb.t ->
  ('h, 'i, 'g) OBus_comb.t ->
  ('j, 'k, 'i) OBus_comb.t ->
  ('l, 'm, 'k) OBus_comb.t ->
  ('n, 'o, 'm) OBus_comb.t ->
  ('p, 'q, 'o) OBus_comb.t ->
  ('r, 's, 'q) OBus_comb.t ->
  ('t, 'u, 's) OBus_comb.t ->
  ('a * 'd * 'f * 'h * 'j * 'l * 'n * 'p * 'r * 't, 'u, 'c) OBus_comb.t
