(*
 * data.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Wire
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

let buffer = String.make 160 '\x00'

let read bo =
  let oc = Unix.open_process_out "xxd" in
  output_string oc buffer;
  close_out oc;
  let oc = Unix.open_process_out "camlp4o -impl /dev/stdin" in
  Printf.fprintf oc "let result = %s"
    (string_of_single
       (snd (match bo with
               | Little_endian ->
                   let module M = Make_unsafe_reader(Little_endian) in
                   M.rvariant buffer 0
               | Big_endian ->
                   let module M = Make_unsafe_reader(Big_endian) in
                   M.rvariant buffer 0)));
  close_out oc

let runw bo x = match bo with
  | Little_endian ->
      let module M = Make_unsafe_writer(Little_endian) in
      M.wvariant buffer 0 x
  | Big_endian ->
      let module M = Make_unsafe_writer(Big_endian) in
      M.wvariant buffer 0 x

let test bo =
  ignore (runw bo data_val);
  read bo

let testc typ =
  make_func typ
    (fun s ->
       ignore (runw OBus_info.Little_endian (vstruct s));
       read OBus_info.Little_endian)

let _ =
  test OBus_info.Little_endian;
  test OBus_info.Big_endian;
  testc (tint --> (tstring --> (tlist tpath --> reply tuint)))  1 "coucou" ["/toto/klk"; "/sdfh/iuo"];
  Unix.sleep 1
