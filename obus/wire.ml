(*
 * wire.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type ptr = int
type buffer = string
exception Content_error of string
module Reading = struct
  exception Array_too_big
  exception Invalid_array_size
  exception Invalid_message_size
  exception Invalid_signature
  exception Unexpected_signature
  exception Unexpected_key
end
module Writing = struct
  exception Array_too_big
end
module type Writer = sig
  type 'a t = buffer -> ptr -> 'a -> unit
  val int_int16 : int t
  val int_int32 : int t
  val int_int64 : int t
  val int_uint16 : int t
  val int_uint32 : int t
  val int_uint64 : int t
  val int32_int32 : int32 t
  val int64_int64 : int64 t
  val int32_uint32 : int32 t
  val int64_uint64 : int64 t
  val float_double : float t
end
module type Reader = sig
  type 'a t = buffer -> ptr -> 'a
  val int_int16 : int t
  val int_int32 : int t
  val int_int64 : int t
  val int_uint16 : int t
  val int_uint32 : int t
  val int_uint64 : int t
  val int32_int32 : int32 t
  val int64_int64 : int64 t
  val int32_uint32 : int32 t
  val int64_uint64 : int64 t
  val float_double : float t
end

let realloc_buffer buffer len =
  let new_buffer = String.create (String.length buffer * 2) in
    String.unsafe_blit buffer 0 new_buffer 0 len;
    new_buffer

include Wire_backend
