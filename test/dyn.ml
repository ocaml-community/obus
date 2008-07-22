(*
 * dyn.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_types
open OBus_value

let v = make_single (tarray tint32) [1l; 2l; 3l]

let s = make_sequence (tup4 tint16 tstring tbyte (tarray tint16)) (1, "toto", 'e', [1; 2])
let (w, (x, (y, z))) = cast_sequence (tcons tint16 (tcons tstring (tup2 tbyte (tarray tint16)))) s

let _ =
  Printf.printf "v : %s = %s\n" (OBus_types.string_of_single (type_of_single v)) (string_of_single v);
  Printf.printf "%ld\n"
    ((cast_sequence tint32)
       [make_single tint32 1l])
