(*
 * gen_random.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_value
open OBus_message

let _ = Random.self_init ()

let option f = if Random.bool () then Some (f ()) else None

(* Generate a random non-empty string *)
let string max_len =
  let len = 1 + Random.int max_len in
  let str = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set str i (char_of_int (Char.code 'a' + Random.int 26))
  done;
  Bytes.unsafe_to_string str

(* Generate an object path *)
let path () =
  let rec aux acc = function 0 -> acc | n -> aux (string 30 :: acc) (n - 1) in
  aux [] (Random.int 10)

(* Generate a valid (interface/bus/error) name *)
let name () =
  let rec aux acc = function 0 -> acc | n -> aux (string 15 :: acc) (n - 1) in
  String.concat "." (aux [] (2 + Random.int 8))

let unique_name () = ":" ^ name ()

(* Generate a valid member name *)
let member () = string 20

let serial () = Random.int32 Int32.max_int

let message_type () =
  match Random.int 4 with
  | 0 -> Method_call (path (), name (), member ())
  | 1 -> Method_return (serial ())
  | 2 -> Error (serial (), name ())
  | _ -> Signal (path (), name (), member ())

let uint16 () = Random.int (1 lsl 16)

let uint32 () =
  Int32.logor
    (Int32.shift_left (Random.int32 Int32.max_int) 1)
    (Random.int32 2l)

let uint64 () =
  Int64.logor
    (Int64.shift_left (Random.int64 Int64.max_int) 1)
    (Random.int64 2L)

let int16 () = uint16 () - (1 lsl 15)

let int32 () = uint32 ()

let int64 () = uint64 ()

let double () = Int64.to_float (int64 ())

(* In the following functions, [count] is the number of terminals
   (basic types/values) and [deep] is the current number of containers
   nesting *)

let tbasic count deep =
  match Random.int 12 with
  | 0 -> (count + 1, T.Byte)
  | 1 -> (count + 1, T.Boolean)
  | 2 -> (count + 1, T.Int16)
  | 3 -> (count + 1, T.Int32)
  | 4 -> (count + 1, T.Int64)
  | 5 -> (count + 1, T.Uint16)
  | 6 -> (count + 1, T.Uint32)
  | 7 -> (count + 1, T.Uint64)
  | 8 -> (count + 1, T.Double)
  | 9 -> (count + 1, T.String)
  | 10 -> (count + 1, T.Signature)
  | _ -> (count + 1, T.Object_path)

let rec tsingle count deep =
  if deep > 3 then
    let count, t = tbasic count deep in
    (count, T.basic t)
  else
    match Random.int 5 with
    | 0 ->
        let count, t = tbasic count deep in
        (count, T.Basic t)
    | 1 ->
        let count, t = tsequence count (deep + 1) in
        (count, T.Structure t)
    | 2 ->
        let count, t = tsingle count (deep + 1) in
        (count, T.Array t)
    | 3 ->
        let count, tk = tbasic count (deep + 1) in
        let count, tv = tsingle count (deep + 1) in
        (count, T.Dict (tk, tv))
    | _ -> (count + 1, T.Variant)

and tsequence count deep =
  let rec aux count acc = function
    | 0 -> (count, acc)
    | n ->
        let count, t = tsingle count (deep + 1) in
        aux count (t :: acc) (n - 1)
  in
  if count > 30 then
    let count, t = tbasic count deep in
    (count, [ T.Basic t ])
  else aux count [] (1 + Random.int 10)

let basic count deep = function
  | T.Byte -> (count + 1, V.Byte (char_of_int (Random.int 256)))
  | T.Boolean -> (count + 1, V.Boolean (Random.bool ()))
  | T.Int16 -> (count + 1, V.Int16 (int16 ()))
  | T.Int32 -> (count + 1, V.Int32 (int32 ()))
  | T.Int64 -> (count + 1, V.Int64 (int64 ()))
  | T.Uint16 -> (count + 1, V.Uint16 (uint16 ()))
  | T.Uint32 -> (count + 1, V.Uint32 (uint32 ()))
  | T.Uint64 -> (count + 1, V.Uint64 (uint64 ()))
  | T.Double -> (count + 1, V.Double (double ()))
  | T.String -> (count + 1, V.String (string 100))
  | T.Signature -> (count + 1, V.Signature (snd (tsequence 0 0)))
  | T.Object_path -> (count + 1, V.Object_path (path ()))
  | T.Unix_fd -> (count + 1, V.Unix_fd Unix.stdin)

let rec single count deep = function
  | T.Basic t ->
      let count, x = basic count deep t in
      (count, V.basic x)
  | T.Structure tl ->
      let count, x = sequence count (deep + 1) tl in
      (count, V.structure x)
  | T.Array t ->
      let rec aux count acc = function
        | 0 -> (count, V.array t acc)
        | n ->
            let count, x = single count (deep + 1) t in
            aux count (x :: acc) (n - 1)
      in
      aux count [] (Random.int (max 1 (min 200 (1000 - count))))
  | T.Dict (tk, tv) ->
      let rec aux count acc = function
        | 0 -> (count, V.dict tk tv acc)
        | n ->
            let count, k = basic count (deep + 1) tk in
            let count, v = single count (deep + 1) tv in
            aux count ((k, v) :: acc) (n - 1)
      in
      aux count [] (Random.int (max 1 (min 200 (1000 - count))))
  | T.Variant ->
      let _, t = tsingle 15 (deep + 1) in
      let count, x = single count (deep + 1) t in
      (count, V.variant x)

and sequence count deep tl =
  List.fold_right
    (fun t (count, l) ->
      let count, x = single count (deep + 1) t in
      (count, x :: l))
    tl (count, [])

let message () =
  {
    flags =
      { no_reply_expected = Random.bool (); no_auto_start = Random.bool () };
    serial = serial ();
    typ = message_type ();
    destination = name ();
    sender = unique_name ();
    body = snd (sequence 0 0 (snd (tsequence 0 0)));
  }
