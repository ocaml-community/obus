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
open OBus_type
open OBus_internals

let typ = Tstruct [Tbasic Tint32;
                   Tbasic Tstring;
                   Tdict(Tstring, Tbasic Tstring);
                   Tarray(Tstruct [Tbasic Tuint64;
                                   Tbasic Tbyte])]

let (>>) m f ctx i = f ctx (m ctx i)

let writer (a, b, c, d) =
  wfixed typ
    (wstruct
       (wint a
        >> wstring b
        >> wassoc wstring wstring c
        >> wlist (Tstruct[Tbasic Tuint64; Tbasic Tbyte]) (fun (x, y) -> wstruct (wuint64 x >> wbyte y)) d))

let reader x = ty_reader tvariant x

let data = (1, "coucou",
            [("truc", "machin"); ("titi", "toto")],
            [(42L, 'r'); (0x342b3daL, 't');
             (-21334234565464L, 'k');
             (0xfedcba987654321L, 'e')])

let buffer = String.make 160 '\x00'

let run w bo p = w { connection = Obj.magic 0;
                     buffer= buffer;
                     bus_name = None;
                     byte_order = bo } p

let read bo =
  let oc = Unix.open_process_out "xxd" in
  output_string oc buffer;
  close_out oc;
  let oc = Unix.open_process_out "camlp4o -impl /dev/stdin" in
  Printf.fprintf oc "let result = %s"
    (string_of_single
       (snd (run reader bo 0)));
  close_out oc

let test bo =
  ignore (run (writer data) bo 0);
  read bo

let testc comb =
  ty_function_send comb
    (fun w ->
       ignore (run (wsignature [Tstruct (isignature comb)] >> w) OBus_info.Little_endian 0);
       read OBus_info.Little_endian)

let _ =
  test OBus_info.Little_endian;
  test OBus_info.Big_endian;
  testc (tint --> (tstring --> (tlist tpath --> reply tuint)))  1 "coucou" ["/toto/klk"; "/sdfh/iuo"];
  Unix.sleep 1
