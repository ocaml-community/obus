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
open OBus_pervasives

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

let read bo =
  let oc = Unix.open_process_out "xxd" in
    output_string oc buffer;
    close_out oc;
    let oc = Unix.open_process_out "camlp4o -impl /dev/stdin" in
      Printf.fprintf oc "let result = %s"
        (OBus_value.string_of_single
           (snd (run reader bo buffer 0)));
      close_out oc

let test bo =
  ignore (run (writer data) bo buffer 0);
  read bo

let testc comb =
  OBus_comb.func_send comb (return ())
    (fun w ->
       let i, _ = run (wsignature [OBus_types.Tstruct
                                     (sequence_type_of_ext
                                        (OBus_comb.func_signature comb))]
                       >> wstruct (return ()))
         Little_endian buffer 0 in
         ignore (run w Little_endian buffer i);
         read Little_endian)

let _ =
  test Little_endian;
  test Big_endian;
  testc (ob_int --> (ob_string --> (ob_list ob_path --> ob_reply ob_uint)))  1 "coucou" ["/toto/klk"; "/sdfh/iuo"];
  Unix.sleep 1
