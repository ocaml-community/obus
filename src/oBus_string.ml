(*
 * oBus_string.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type t = string

type error = {
  typ : string;
  str : string;
  ofs : int;
  msg : string;
} with projection

type validator = string -> error option

exception Invalid_string of error

let error_message error =
  if error.ofs < 0 then
    Printf.sprintf "invalid DBus %s(%S): %s" error.typ error.str error.msg
  else
    Printf.sprintf "invalid DBus %s(%S), at position %d: %s" error.typ error.str error.ofs error.msg

let validate s =
  let fail i msg = Some{ typ = "string"; str = s; ofs = i; msg = msg } in

  let rec trail minimum acc count i =
    if i = String.length s then
      fail i "premature end of UTF8 sequence"
    else
      let n = Char.code (String.unsafe_get s i) in
      if n land 0xc0 = 0x80 then
        let acc = (acc lsl 6) lor (n land 0x3f) in
        match count with
          | 0 ->
              if acc < minimum then
                fail i "overlong UTF8 sequence"
              else
                main (i + 1)
          | _ ->
              trail minimum acc (count - 1) (i + 1)
      else
        fail i "malformed UTF8 sequence"

  and main i =
    if i = String.length s then
      None
    else
      let n = Char.code (String.unsafe_get s i) in
      if n = 0 then
        fail i "null byte"
      else if n land 0x80 = 0 then
        main (i + 1)
      else if n land 0xe0 = 0xc0 then
        trail 0x80 (n land 0x1f) 1 i
      else if n land 0xf0 = 0xe0 then
        trail 0x800 (n land 0x0f) 2 i
      else if n land 0xf8 = 0xf0 then
        trail 0x10000 (n land 0x07) 3 i
      else
        fail i "invalid start of UTF8 sequence"
  in

  main 0

let assert_validate validator str = match validator str with
  | Some error -> raise (Invalid_string error)
  | None -> ()
