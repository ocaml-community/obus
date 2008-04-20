(*
 * address.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type key = string
type value = string
type name = string

type parameter = key * value
type t = name * parameter list

exception Parse_error of string

let default_system_bus_address = "unix:path=/var/run/dbus/system_bus_socket"

(* optionnally escaped: [0-9A-Za-z_-/.\] *)
let optionnally_escaped ch =
  (ch >= '0' && ch <= '9') ||
    (ch >= 'A' && ch <= 'Z') ||
    (ch >= 'a' && ch <= 'z') ||
    (List.mem ch ['_'; '-'; '/'; '.'; '\\'])

let split string char =
  let i = String.index string char in
    (String.sub string 0 i,
     String.sub string (i + 1) (String.length string - i - 1))

let int_of_hex = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' | 'a' -> 10
  | 'B' | 'b' -> 11
  | 'C' | 'c' -> 12
  | 'D' | 'd' -> 13
  | 'E' | 'e' -> 14
  | 'F' | 'f' -> 15
  | _ -> raise (Parse_error "non hex-digit in address")

let char_of_hex string pos =
  char_of_int ((int_of_hex string.[pos] lsl 4) lor
                 (int_of_hex string.[pos + 1]))

let unescape_value string =
  let len = String.length string in
  let value = String.create len in
  let isrc = ref 0 and idst = ref 0 in
    while !isrc < len do
      begin match string.[!isrc] with
        | '%' ->
            value.[!idst] <- char_of_hex string (!isrc + 1);
            isrc := !isrc + 3
        | char when optionnally_escaped char ->
            value.[!idst] <- char;
            incr isrc
        | _ -> raise (Parse_error "invalid character in address")
      end;
      incr idst
    done;
    value

let parse_keyvalue str =
  let key, value = split str '=' in
    (key,
     unescape_value value)

let parse_method str =
  let name, keyvalues = split str ':' in
    (name,
     List.map parse_keyvalue (Str.split (Str.regexp ",") keyvalues))

let of_string str =
    List.map parse_method (Str.split (Str.regexp ";") str)

let get_method = fst

let get_param (_, l) param =
  try
    Some (List.assoc param l)
  with
    | Not_found -> None

let system () =
  of_string
    (try Unix.getenv "DBUS_SYSTEM_BUS_ADDRESS" with
         Not_found -> default_system_bus_address)

let session () =
  try of_string (Unix.getenv "DBUS_SESSION_BUS_ADDRESS") with
      Not_found -> []
