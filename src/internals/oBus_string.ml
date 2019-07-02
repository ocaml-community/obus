(*
 * oBus_string.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type t = string

type error = {
  typ : string;
  str : string;
  ofs : int;
  msg : string;
}

let typ e = e.typ
let str e = e.str
let ofs e = e.ofs
let msg e = e.msg

type validator = string -> error option

exception Invalid_string of error

let error_message error =
  if error.ofs < 0 then
    Printf.sprintf "invalid D-Bus %s (%S): %s" error.typ error.str error.msg
  else
    Printf.sprintf "invalid D-Bus %s (%S), at position %d: %s" error.typ error.str error.ofs error.msg

let () =
  Printexc.register_printer
    (function
       | Invalid_string error ->
           Some(error_message error)
       | _ ->
           None)

let () =
  Printexc.register_printer
    (function
       | Invalid_string error ->
           Some(error_message error)
       | _ ->
           None)

let validate s =
  let fail i msg = Some{ typ = "string"; str = s; ofs = i; msg = msg } in
  let len = String.length s in
  let rec main i =
    if i = len then
      None
    else
      let ch = String.unsafe_get s i in
      match ch with
        | '\x00' ->
            fail i "null byte"
        | '\x01' .. '\x7f' ->
            main (i + 1)
        | '\xc0' .. '\xdf' ->
            if i + 1 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x1f) lsl 6) lor (byte1 land 0x3f) < 0x80 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 2)
            end
        | '\xe0' .. '\xef' ->
            if i + 2 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1))
              and byte2 = Char.code (String.unsafe_get s (i + 2)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if byte2 land 0xc0 != 0x80 then
                fail (i + 2) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x0f) lsl 12) lor ((byte1 land 0x3f) lsl 6) lor (byte2 land 0x3f) < 0x800 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 3)
            end
        | '\xf0' .. '\xf7' ->
            if i + 3 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1))
              and byte2 = Char.code (String.unsafe_get s (i + 2))
              and byte3 = Char.code (String.unsafe_get s (i + 3)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if byte2 land 0xc0 != 0x80 then
                fail (i + 2) "malformed UTF8 sequence"
              else if byte3 land 0xc0 != 0x80 then
                fail (i + 3) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x0f) lsl 18) lor ((byte1 land 0x3f) lsl 12) lor ((byte2 land 0x3f) lsl 6) lor (byte3 land 0x3f) < 0x10000 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 4)
            end
        | _ ->
            fail i "invalid start of UTF8 sequence"
  in
  main 0

let assert_validate validator str = match validator str with
  | Some error -> raise (Invalid_string error)
  | None -> ()
