(*
 * data.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_lowlevel
open OBus_type
open OBus_value
open OBus_info

let typ = <:obus_type< [int * string * {string, string} list * [uint64 * byte] list] >>

let data = (1, "coucou",
            [("truc", "machin"); ("titi", "toto")],
            [(42L, 'r'); (0x342b3daL, 't');
             (-21334234565464L, 'k');
             (0xfedcba987654321L, 'e')])

let data_val = make_single typ data

let make_msg x = OBus_message.method_call ~path:["plop"; "plip"] ~member:"truc" x

let pipe_to_cmd cmd f =
  let ic, oc = Unix.open_process cmd in
  f oc;
  close_out oc;
  try
    while true do
      print_endline (input_line ic)
    done
  with
      End_of_file ->
        ignore (Unix.close_process (ic, oc))

type ptr = { buffer : string; mutable offset : int }

module Ptr =
struct
  type 'a t = ptr -> 'a
  let bind m f ptr = f (m ptr) ptr
  let return x _ = x
  let fail exn ptr = Printf.eprintf "failure at byte %x\n%!" (ptr.offset - 1); raise exn
end

module Reader = Make_reader
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

module Writer = Make_writer
  (struct
     include Ptr
     let put_char ch ptr =
       let n = ptr.offset in
       String.unsafe_set ptr.buffer n ch;
       ptr.offset <- n + 1
     let put_string str ptr =
       let n = ptr.offset and len = String.length str in
       String.unsafe_blit str 0 ptr.buffer n len;
       ptr.offset <- n + len
   end)

let read buffer =
  pipe_to_cmd "xxd" (fun oc -> output_string oc buffer);
  pipe_to_cmd "camlp4o -impl /dev/stdin -printer Camlp4Printers/Camlp4OCamlPrinter.cmo"
    (fun oc ->
       Printf.fprintf oc "let result = %s"
         (string_of_sequence
            (Reader.get_message { buffer = buffer; offset = 0 }).OBus_message.body))

let write bo x =
  let len, writer = Writer.put_message ~byte_order:bo (make_msg x :> OBus_message.any) in
  let buffer = String.create len in
  writer { buffer = buffer; offset = 0 };
  buffer

let test bo = read (write bo [data_val])

let testc typ = make_func typ (fun s -> read (write OBus_info.Little_endian s))

let _ =
  test OBus_info.Little_endian;
  test OBus_info.Big_endian;
  testc (tint --> (tstring --> (tlist tpath --> reply tuint)))  1 "coucou" [["toto"; "klk"]; ["sdfh"; "iuo"]]
