(*
 * oBus_pervasives.mli
 * -------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Pervasives type combinators *)

open OBus_type

(** {6 Type combinators for basic types} *)

val obus_byte : char basic
val obus_char : char basic
val obus_boolean : bool basic
val obus_bool : bool basic
val obus_int8 : int basic
val obus_int16 : int basic
val obus_int : int basic
val obus_int32 : int32 basic
val obus_int64 : int64 basic
val obus_uint8 : int basic
val obus_uint16 : int basic
val obus_uint : int basic
val obus_uint32 : int32 basic
val obus_uint64 : int64 basic
val obus_double : float basic
val obus_float : float basic
val obus_string : string basic
val obus_signature : OBus_value.signature basic
val obus_object_path : OBus_path.t basic
val obus_path : OBus_path.t basic
val obus_file_descr : Lwt_unix.file_descr basic
val obus_unix_file_descr : Unix.file_descr basic

(** {6 Type combinators for container types} *)

val obus_list : ('a, _) cl_single -> 'a list container
val obus_array : ('a, _) cl_single -> 'a array container
val obus_byte_array : string container
val obus_dict : ('a, _) cl_basic -> ('b, _) cl_single -> (('a * 'b) list) container
val obus_structure : ('a, _) cl_sequence -> 'a container
val obus_variant : OBus_value.single container

(** {6 Type combinators for sequences} *)

val obus_unit : unit sequence

(** {6 Broken types} *)

(** It often happen that people confuse string and object-path, and
    use strings where they should use object pathes. *)

val obus_broken_path : OBus_path.t basic
  (** Type combinator with caml type {!OBus_path.t} and D-Bus type
      [STRING]. *)

(** {6 Dummy types} *)

(** Dummy type definition, they should be used in combination with the
    syntax extension, to define the dbus type and the caml type at the
    same time *)

type byte = char
type boolean = bool
type int8 = int
type uint8 = int
type int16 = int
type uint16 = int
type uint32 = int32
type uint64 = int64
type uint = int
type double = float
type signature = OBus_value.signature
type object_path = OBus_path.t
type path = OBus_path.t
type ('a, 'b) dict = ('a * 'b) list
type 'a structure = 'a
type variant = OBus_value.single
type byte_array = string
type file_descr = Lwt_unix.file_descr
type unix_file_descr = Unix.file_descr
type broken_path = OBus_path.t
