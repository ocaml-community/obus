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

let buffer = String.make 300 '\x00'

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

let read () =
  pipe_to_cmd "xxd" (fun oc -> output_string oc buffer);
  pipe_to_cmd "camlp4o -impl /dev/stdin -printer Camlp4Printers/Camlp4OCamlPrinter.cmo"
    (fun oc ->
       Printf.fprintf oc "let result = %s"
         (string_of_sequence
            (let i = ref 0 in
             let ic = Lwt_chan.make_in_channel
               (fun str pos sz ->
                  let sz = min sz (String.length buffer - !i) in
                  String.blit buffer !i str pos sz;
                  i := !i + sz;
                  return sz) in
             Lwt_unix.run (Lwt_wire.get_message (lwt_input_of_channel ic))).OBus_message.body))

let write bo x =
  let i = ref 0 in
  let oc = Lwt_chan.make_out_channel
    (fun str pos sz ->
       let sz = min sz (String.length buffer - !i) in
       String.blit str pos buffer !i sz;
       i := !i + sz;
       return sz) in
  Lwt_unix.run (perform
                  snd (Lwt_wire.put_message ~byte_order:bo (make_msg x :> OBus_message.any)) (lwt_output_of_channel oc);
                  Lwt_chan.flush oc)

let test bo =
  write bo [data_val];
  read ()

let testc typ =
  make_func typ
    (fun s ->
       write OBus_info.Little_endian s;
       read ())

let _ =
  test OBus_info.Little_endian;
  test OBus_info.Big_endian;
  testc (tint --> (tstring --> (tlist tpath --> reply tuint)))  1 "coucou" [["toto"; "klk"]; ["sdfh"; "iuo"]]
