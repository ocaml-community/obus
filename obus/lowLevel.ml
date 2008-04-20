(*
 * lowLevel.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type S = sig
  type bus

  type buffer = string

  module type BufferType = sig val buffer : buffer end

  type ptr = int
    (** A position in a buffer *)

  type 'a reader = ptr -> ptr * 'a
  type 'a writer = 'a -> ptr -> ptr

  (** {6 Byte order} *)

  module type ByteOrder

  module LittleEndian : ByteOrder
  module BigEndian : ByteOrder

  module T : Types.S
  module V : Values.S

  (** {6 Reading dbus values} *)

  exception Read_error of string

  module Reader(BO : ByteOrder)(Buffer : BufferType) : sig
    val pad2 : ptr -> ptr
    val pad4 : ptr -> ptr
    val pad8 : ptr -> ptr
    val read_byte : char reader
    val read_boolean : bool reader
    val read_int16 : int reader
    val read_int32 : int reader
    val read_int64 : int reader
    val read_int32_as_int32 : int32 reader
    val read_int64_as_int64 : int64 reader
    val read_uint16 : int reader
    val read_uint32 : int reader
    val read_uint64 : int reader
    val read_uint32_as_int32 : int32 reader
    val read_uint64_as_int64 : int64 reader
    val read_double : float reader
    val read_string : string reader
    val read_signature : T.t reader
    val read_signature_as_string : string reader
    val read_object_path : string reader
    val read_array : 'a reader -> 'b -> ('a -> 'b -> 'b) -> 'b reader
    val read_dict : 'a reader -> 'b reader -> 'c -> ('a -> 'b -> 'c -> 'c) -> 'c reader
    val read_structure : 'a reader -> 'a reader
    val read_variant : V.single reader
    val read_fixed_variant : string -> 'a reader -> 'a reader
  end

  (** {6 Writing dbus values} *)

  exception Write_error of string

  module Writer(BO : ByteOrder)(Buffer : BufferType) : sig
    val pad2 : ptr -> ptr
    val pad4 : ptr -> ptr
    val pad8 : ptr -> ptr
    val write_byte : char writer
    val write_boolean : bool writer
    val write_int16 : int writer
    val write_int32 : int writer
    val write_int64 : int writer
    val write_int32_from_int32 : int32 writer
    val write_int64_from_int64 : int64 writer
    val write_uint16 : int writer
    val write_uint32 : int writer
    val write_uint64 : int writer
    val write_uint32_from_int32 : int32 writer
    val write_uint64_from_int64 : int64 writer
    val write_double : float writer
    val write_string : string writer
    val write_signature : T.t writer
    val write_signature_from_string : string writer
    val write_object_path : string writer
    val write_array : 'a writer -> (('a writer) -> ptr -> 'b -> ptr) -> 'b writer
    val write_dict : 'a writer -> 'b writer -> (('a -> 'b writer) -> ptr -> 'c -> ptr) -> 'c writer
    val write_structure : 'a writer -> 'a writer
    val write_variant : V.single writer
    val write_fixed_variant : string -> 'a writer -> 'a writer
  end

  exception Connection_failed

  val open_bus : Address.t list -> bus

  (** {6 Communication} *)

  val send_message : bus -> Header.fields list -> string -> string
    -> (Header.byte_order -> buffer -> ptr -> ptr) -> (Header.byte_order -> buffer -> 'b reader) -> 'b
    (** [send_method_call bus hfields mess_sig repl_sig writer reader]
        use [writer] to write a message and send it through
        [bus]. When the reply came it is read with [reader]. *)
end

module M : S = LowLevel_internal
