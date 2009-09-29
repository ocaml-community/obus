(*
 * oBus_uuid.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type t = string

let of_string str =
  let fail _ = raise (Invalid_argument (Printf.sprintf "OBus_uuid.of_string(%S)" str)) in
  if String.length str <> 32 then fail ();
  try OBus_util.hex_decode str
  with _ -> fail ()

let to_string = OBus_util.hex_encode

let generate () =
  let uuid = String.create 16 in
  OBus_util.fill_random uuid 0 12;
  let v = Int32.of_float (Unix.time ()) in
  uuid.[12] <- (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24)));
  uuid.[13] <- (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16)));
  uuid.[14] <- (Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8)));
  uuid.[15] <- (Char.unsafe_chr (Int32.to_int v));
  uuid

let obus_t = OBus_type.map OBus_type.Perv.obus_string of_string to_string
