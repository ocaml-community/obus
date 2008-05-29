(*
 * wire_backend_c.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

external native_byte_order : unit -> int = "caml_native_byte_order"
module LEWriter = struct
  type 'a t = string -> int -> 'a -> unit
  external int_int16 : string -> int -> int -> unit = "caml_writer_le_int_int16"
  external int_int32 : string -> int -> int -> unit = "caml_writer_le_int_int32"
  external int_int64 : string -> int -> int -> unit = "caml_writer_le_int_int64"
  external int_uint16 : string -> int -> int -> unit = "caml_writer_le_int_uint16"
  external int_uint32 : string -> int -> int -> unit = "caml_writer_le_int_uint32"
  external int_uint64 : string -> int -> int -> unit = "caml_writer_le_int_uint64"
  external int32_int32 : string -> int -> int32 -> unit = "caml_writer_le_int32_int32"
  external int64_int64 : string -> int -> int64 -> unit = "caml_writer_le_int64_int64"
  external int32_uint32 : string -> int -> int32 -> unit = "caml_writer_le_int32_uint32"
  external int64_uint64 : string -> int -> int64 -> unit = "caml_writer_le_int64_uint64"
  external float_double : string -> int -> float -> unit = "caml_writer_le_float_double"
end
module BEWriter = struct
  type 'a t = string -> int -> 'a -> unit
  external int_int16 : string -> int -> int -> unit = "caml_writer_be_int_int16"
  external int_int32 : string -> int -> int -> unit = "caml_writer_be_int_int32"
  external int_int64 : string -> int -> int -> unit = "caml_writer_be_int_int64"
  external int_uint16 : string -> int -> int -> unit = "caml_writer_be_int_uint16"
  external int_uint32 : string -> int -> int -> unit = "caml_writer_be_int_uint32"
  external int_uint64 : string -> int -> int -> unit = "caml_writer_be_int_uint64"
  external int32_int32 : string -> int -> int32 -> unit = "caml_writer_be_int32_int32"
  external int64_int64 : string -> int -> int64 -> unit = "caml_writer_be_int64_int64"
  external int32_uint32 : string -> int -> int32 -> unit = "caml_writer_be_int32_uint32"
  external int64_uint64 : string -> int -> int64 -> unit = "caml_writer_be_int64_uint64"
  external float_double : string -> int -> float -> unit = "caml_writer_be_float_double"
end
module LEReader = struct
  type 'a t = string -> int -> 'a
  external int_int16 : string -> int -> int = "caml_reader_le_int_int16"
  external int_int32 : string -> int -> int = "caml_reader_le_int_int32"
  external int_int64 : string -> int -> int = "caml_reader_le_int_int64"
  external int_uint16 : string -> int -> int = "caml_reader_le_int_uint16"
  external int_uint32 : string -> int -> int = "caml_reader_le_int_uint32"
  external int_uint64 : string -> int -> int = "caml_reader_le_int_uint64"
  external int32_int32 : string -> int -> int32 = "caml_reader_le_int32_int32"
  external int64_int64 : string -> int -> int64 = "caml_reader_le_int64_int64"
  external int32_uint32 : string -> int -> int32 = "caml_reader_le_int32_uint32"
  external int64_uint64 : string -> int -> int64 = "caml_reader_le_int64_uint64"
  external float_double : string -> int -> float = "caml_reader_le_float_double"
end
module BEReader = struct
  type 'a t = string -> int -> 'a
  external int_int16 : string -> int -> int = "caml_reader_be_int_int16"
  external int_int32 : string -> int -> int = "caml_reader_be_int_int32"
  external int_int64 : string -> int -> int = "caml_reader_be_int_int64"
  external int_uint16 : string -> int -> int = "caml_reader_be_int_uint16"
  external int_uint32 : string -> int -> int = "caml_reader_be_int_uint32"
  external int_uint64 : string -> int -> int = "caml_reader_be_int_uint64"
  external int32_int32 : string -> int -> int32 = "caml_reader_be_int32_int32"
  external int64_int64 : string -> int -> int64 = "caml_reader_be_int64_int64"
  external int32_uint32 : string -> int -> int32 = "caml_reader_be_int32_uint32"
  external int64_uint64 : string -> int -> int64 = "caml_reader_be_int64_uint64"
  external float_double : string -> int -> float = "caml_reader_be_float_double"
end
external string_match : string -> int -> string -> int -> bool = "caml_string_match"
