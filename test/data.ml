(*
 * data.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_wire
open OBus_annot

let typ = dstruct (dint ++ dstring ++ ddict dstring dstring ++ darray (dstruct (duint64 ++ dbyte)))

let writer (a, b, c, d) =
  wfixed typ
    (wstruct
       (perform
          wint a;
          wstring b;
          wassoc wstring wstring c;
          wlist (dstruct (duint64 ++ dbyte))
            (fun (x, y) -> wstruct (wuint64 x >> wbyte y)) d))

let reader = rvariant

let data = (1, "coucou",
            [("truc", "machin"); ("titi", "toto")],
            [(42L, 'r'); (0x342b3daL, 't');
             (-21334234565464L, 'k');
             (0xfedcba987654321L, 'e')])

let buffer = String.make 160 '\x00'

let test bo =
  ignore (run (writer data) bo buffer 0);
  let oc = Unix.open_process_out "xxd" in
    output_string oc buffer;
    close_out oc;
    let oc = Unix.open_process_out "camlp4o -impl /dev/stdin" in
      Printf.fprintf oc "let result = %s"
        (OBus_value.string_of_single
           (snd (run reader bo buffer 0)));
      close_out oc

let _ =
  test Little_endian;
  test Big_endian;
  Unix.sleep 1
