(*
 * oBus_uuid.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type t = string

let of_string str =
  let fail _ = raise (Invalid_argument (Printf.sprintf "OBus_uuid.of_string(%S)" str)) in
  if String.length str <> 32 then fail ();
  try OBus_util.hex_decode str
  with _ -> fail ()

let to_string = OBus_util.hex_encode

let generate () =
  let uuid = Bytes.create 16 in
  OBus_util.fill_random uuid 0 12;
  let v = Int32.of_float (Unix.time ()) in
  Bytes.set uuid 12 (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24)));
  Bytes.set uuid 13 (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
  Bytes.set uuid 14 (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
  Bytes.set uuid 15 (Char.unsafe_chr (Int32.to_int v));
  Bytes.unsafe_to_string uuid
