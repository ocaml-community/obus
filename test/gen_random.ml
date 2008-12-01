(*
 * gen_random.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_value
open OBus_message

let _ = Random.self_init ()

let option f =
  if Random.bool () then
    Some(f ())
  else
    None

(* Generate a random non-empty string *)
let string max_len =
  let len = 1 + Random.int max_len in
  let str = String.create len in
  for i = 0 to len - 1 do
    str.[i] <- char_of_int (Char.code 'a' + Random.int 26)
  done;
  str

(* Generate an object path *)
let path () =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (string 30 :: acc) (n - 1)
  in
  aux [] (Random.int 10)

(* Generate a valid (interface/bus/error) name *)
let name () =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (string 15 :: acc) (n - 1)
  in
  String.concat "." (aux [] (2 + Random.int 8))

let unique_name () = ":" ^ name ()

(* Generate a valid member name *)
let member () = string 20

let serial () = Random.int32 Int32.max_int

let message_type () = match Random.int 4 with
  | 0 -> `Method_call(path (), option name, member ())
  | 1 -> `Method_return(serial ())
  | 2 -> `Error(serial (), name ())
  | _ -> `Signal(path (), name (), member ())

let uint16 () = Random.int (1 lsl 16)
let uint32 () = Int32.logor (Int32.shift_left (Random.int32 Int32.max_int) 1) (Random.int32 2l)
let uint64 () = Int64.logor (Int64.shift_left (Random.int64 Int64.max_int) 1) (Random.int64 2L)
let int16 () = uint16 () - (1 lsl 15)
let int32 () = uint32 ()
let int64 () = uint64 ()

(* In the following functions, [count] is the number of terminals
   (basic types/values) and [deep] is the current number of containers
   nesting *)

let tbasic count deep = match Random.int 12 with
  | 0 -> count + 1, Tbyte
  | 1 -> count + 1, Tboolean
  | 2 -> count + 1, Tint16
  | 3 -> count + 1, Tint32
  | 4 -> count + 1, Tint64
  | 5 -> count + 1, Tuint16
  | 6 -> count + 1, Tuint32
  | 7 -> count + 1, Tuint64
  | 8 -> count + 1, Tdouble
  | 9 -> count + 1, Tstring
  | 10 -> count + 1, Tsignature
  | _ -> count + 1, Tobject_path

let rec tsingle count deep =
  if deep > 3 then
    let count, t = tbasic count deep in
    (count, Tbasic t)
  else
    match Random.int 4 with
      | 0 -> let count, t = tbasic count deep in (count, Tbasic t)
      | 1 -> let count, t = tsequence count (deep + 1) in (count, Tstruct t)
      | 2 -> let count, t = telement count (deep + 1) in (count, Tarray t)
      | _ -> (count + 1, Tvariant)

and telement count deep = match Random.int 2 with
  | 0 ->
      let count, t = tsingle count deep in
      (count, Tsingle t)
  | _ ->
      let count, tk = tbasic count deep in
      let count, tv = tsingle count deep in
      (count, Tdict_entry(tk, tv))

and tsequence count deep =
  let rec aux count acc = function
    | 0 -> (count, acc)
    | n -> let count, t = tsingle count (deep + 1) in aux count (t :: acc) (n - 1)
  in
  if count > 30 then
    let count, t = tbasic count deep in
    (count, [Tbasic t])
  else
    aux count [] (1 + Random.int 10)

let basic count deep = function
  | Tbyte -> count + 1, Byte(char_of_int (Random.int 256))
  | Tboolean -> count + 1, Boolean(Random.bool ())
  | Tint16 -> count + 1, Int16(int16 ())
  | Tint32 -> count + 1, Int32(int32 ())
  | Tint64 -> count + 1, Int64(int64 ())
  | Tuint16 -> count + 1, Uint16(uint16 ())
  | Tuint32 -> count + 1, Uint32(uint32 ())
  | Tuint64 -> count + 1, Uint64(uint64 ())
  | Tdouble -> count + 1, Double(Int64.float_of_bits (int64 ()))
  | Tstring -> count + 1, String(string 100)
  | Tsignature -> count + 1, Signature(snd (tsequence 0 0))
  | Tobject_path -> count + 1, Object_path(path ())

let rec single count deep = function
  | Tbasic t ->
      let count, x = basic count deep t in
      (count, vbasic x)
  | Tstruct tl ->
      let count, x = sequence count (deep + 1) tl in
      (count, vstruct x)
  | Tarray t ->
      let rec aux count acc = function
        | 0 -> (count, varray t acc)
        | n -> let count, x = element count (deep + 1) t in aux count (x :: acc) (n - 1)
      in
      aux count [] (Random.int (max 1 (min 200 (1000 - count))))
  | Tvariant ->
      let _, t = tsingle 15 (deep + 1) in
      let count, x = single count (deep + 1) t in
      (count, vvariant x)

and element count deep = function
  | Tsingle t ->
      let count, x = single count deep t in
      (count, Single x)
  | Tdict_entry(tk, tv) ->
      let count, k = basic count deep tk in
      let count, v = single count deep tv in
      (count, Dict_entry(k, v))

and sequence count deep tl =
  List.fold_right (fun t (count, l) ->
                     let count, x = single count (deep + 1) t in
                     (count, x :: l))
    tl (count, [])

let message () = {
  flags = { no_reply_expected = Random.bool (); no_auto_start = Random.bool () };
  serial = serial ();
  typ = message_type ();
  destination = option name;
  sender = option unique_name;
  body = snd (sequence 0 0 (snd (tsequence 0 0)));
}
