(*
 * oBus_uuid.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type t = string

let decode_char ch = match ch with
  | '0'..'9' -> Char.code ch - Char.code '0'
  | 'a'..'f' -> Char.code ch - Char.code 'a' + 10
  | 'A'..'F' -> Char.code ch - Char.code 'A' + 10
  | _ -> raise (Invalid_argument (Printf.sprintf "OBus_uuid.of_string: invalid char in uuid (%C)" ch))

let of_string str =
  if String.length str <> 32 then raise (Invalid_argument "OBus_uuid.of_string: uuid must be of length 32");
  let uuid = String.create 16 in
  for i = 0 to 15 do
    String.unsafe_set uuid i
      (char_of_int
         ((decode_char (String.unsafe_get str (i * 2)) lsl 4) lor
            (decode_char (String.unsafe_get str (i * 2 + 1)))))
  done;
  uuid

let encode_char n =
  if n < 10 then
    char_of_int (n + Char.code '0')
  else if n < 16 then
    char_of_int (n + Char.code 'a')
  else
    assert false

let to_string uuid =
  let str = String.create 32 in
  for i = 0 to 15 do
    let n = Char.code (String.unsafe_get uuid i) in
    String.unsafe_set str (i * 2) (encode_char (n lsr 4));
    String.unsafe_set str (i * 2 + 1) (encode_char (n land 15))
  done;
  str

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
