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
  try Util.hex_decode str
  with _ -> fail ()

let to_string = Util.hex_encode

let init_pseudo = Lazy.lazy_from_fun Random.self_init

let generate_pseudo uuid n =
  LOG("using pseudo random generator");
  Lazy.force init_pseudo;
  for i = n to 15 do
    String.unsafe_set uuid i (char_of_int (Random.int 256))
  done

let generate () =
  let uuid = String.create 16 in
  begin try
    Util.with_open_in"/dev/urandom"
      (fun ic ->
         let n = input ic uuid 0 16 in
         if n < 16 then generate_pseudo uuid n)
  with
      exn ->
        DEBUG("failed to use the random generator (%S)" (Printexc.to_string exn));
        generate_pseudo uuid 16
  end;
  uuid

let loopback = String.make 16 '\000'
let tt = OBus_type.wrap_basic OBus_type.tstring of_string to_string
