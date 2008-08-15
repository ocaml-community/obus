(*
 * dyn.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type
open OBus_value
open OBus_type

let v = make_single (tlist tint32) [1l; 2l; 3l]

let s = make_sequence <:obus_type< int16 * string * byte * int list >> (1, "toto", 'e', [1; 2])
let (w, (x, (y, z))) = cast_sequence (tpair tint16 (tpair tstring (tup2 tbyte (tlist tint)))) s

module M = Make_map(struct
                      OBUS_type t = string
                      let compare = compare
                    end)

let x = List.fold_left (fun acc (x, y) -> M.add x y acc) M.empty
  ["a", 1;
   "w", 2;
   "sdf", 3;
   "poitop", 42]

module S = Make_set(struct
                      type t = string
                      let tt = (tstring :> t cl_element)
                      let compare = compare
                    end)

let y = List.fold_left (fun acc x -> S.add x acc) S.empty ["sdhf"; "ioreut"; "ioio"]

let _ =
  let oc = Unix.open_process_out "camlp4o -impl /dev/stdin" in
  Printf.fprintf oc "let _ = %s\n\
                     let _ = %s\n\
                     let _ = %s\n"
    (string_of_tsingle (type_of_single v))
    (string_of_single v)
    (string_of_single (make_single <:obus_type< [int M.t * S.t] >> (x, y)));
  Unix.close_process_out oc
