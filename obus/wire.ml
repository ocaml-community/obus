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
module Reading : sig
  exception Array_too_big
  exception Invalid_array_size
  exception Invalid_message_size
  exception Invalid_signature
end
module Writing = struct
  exception Array_too_big
end

let realloc_buffer buffer len =
  let new_buffer = String.create (String.length buffer * 2) in
    String.unsafe_blit buffer 0 new_buffer 0 len;
    new_buffer
