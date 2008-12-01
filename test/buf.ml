(*
 * buf.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Serialization/deserialization with buffers *)

type ptr = { buffer : string; mutable offset : int }

module Ptr =
struct
  type 'a t = ptr -> 'a
  let bind m f ptr = f (m ptr) ptr
  let return x _ = x
  let fail exn ptr = raise exn
end

module Reader = OBus_lowlevel.Make_reader
  (struct
     include Ptr
     let get_char ptr =
       let n = ptr.offset in
       ptr.offset <- n + 1;
       ptr.buffer.[n]
     let get_string len ptr =
       let n = ptr.offset in
       ptr.offset <- n + len;
       String.sub ptr.buffer n len
   end)

module Writer = OBus_lowlevel.Make_writer
  (struct
     include Ptr
     let put_char ch ptr =
       let n = ptr.offset in
       ptr.buffer.[n] <- ch;
       ptr.offset <- n + 1
     let put_string str ptr =
       let n = ptr.offset and len = String.length str in
       String.blit str 0 ptr.buffer n len;
       ptr.offset <- n + len
   end)

let put ?byte_order msg =
  let size, writer = Writer.put_message ?byte_order msg in
  let buf = String.create size in
  writer { buffer = buf; offset = 0 };
  buf

let get buf =
  Reader.get_message { buffer = buf; offset = 0 }
