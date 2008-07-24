(*
 * oBus_comb.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_types
open OBus_wire

type ('a, +'b, +'c) simple = ('b, 'c) annot * ('a, 'b, 'c) reader * ('a -> (unit, 'b, 'c) writer)
type ('a, +'b, +'c) one = ('a, 'b, 'c * 'b) simple
type (+'function_type, 'return_type, +'var) func = ('function_type, 'return_type, 'var) OBus_intern_comb.func

let annot (x, y, z) = x
let reader (x, y, z) = y
let writer (x, y, z) = z
let from_wire ~typ ~reader ~writer = (typ, reader, writer)

let wrap (typ, reader, writer) f g =
  (typ,
   (perform with module Reader in
      x <-- reader;
      return (f x)),
   (fun x -> writer (g x)))

open OBus_intern_conv

let return (typ, reader, writer) = {
  in_signature = [];
  out_signature = sequence_type_of_annot typ;
  send = (* TODO *);
  recv = (* TODO *);
}

let abstract (typ, reader, writer) func =
  { out_signature = func.out_signature;
    in_signature = sequence_type_of_annot typ @ func.in_signature;
    send = (* TODO *);
    recv = (* TODO *) }
