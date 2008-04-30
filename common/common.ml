(*
 * common.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type dtype =
  | Tbyte
  | Tboolean
  | Tint16
  | Tint32
  | Tint64
  | Tuint16
  | Tuint32
  | Tuint64
  | Tdouble
  | Tstring
  | Tsignature
  | Tobject_path
  | Tarray of dtype
  | Tdict of dtype * dtype
  | Tstructure of dtype list
  | Tvariant
type dtypes = dtype list

let rec signature_of_dtype_aux buf = function
  | Tbyte -> Buffer.add_char buf 'y'
  | Tboolean -> Buffer.add_char buf 'b'
  | Tint16 -> Buffer.add_char buf 'n'
  | Tuint16 -> Buffer.add_char buf 'q'
  | Tint32 -> Buffer.add_char buf 'i'
  | Tuint32 -> Buffer.add_char buf 'u'
  | Tint64 -> Buffer.add_char buf 'x'
  | Tuint64 -> Buffer.add_char buf 't'
  | Tdouble -> Buffer.add_char buf 'd'
  | Tstring -> Buffer.add_char buf 's'
  | Tobject_path -> Buffer.add_char buf 'o'
  | Tsignature -> Buffer.add_char buf 'g'
  | Tarray(x) -> Buffer.add_char buf 'a'; signature_of_dtype_aux buf x
  | Tdict(k, v) ->
      Buffer.add_char buf 'a';
      Buffer.add_char buf '{';
      signature_of_dtype_aux buf k;
      signature_of_dtype_aux buf v;
      Buffer.add_char buf '}'
  | Tstructure(l) ->
      Buffer.add_char buf '(';
      List.iter (signature_of_dtype_aux buf) l;
      Buffer.add_char buf ')'
  | Tvariant -> Buffer.add_char buf 'v'

let signature_of_dtype dtype =
  let buf = Buffer.create 42 in
    signature_of_dtype_aux buf dtype;
    Buffer.contents buf

let signature_of_dtypes dtypes =
  let buf = Buffer.create 42 in
    List.iter (signature_of_dtype_aux buf) dtypes;
    Buffer.contents buf

let basic_of_char = function
  | 'y' -> Tbyte
  | 'b' -> Tboolean
  | 'n' -> Tint16
  | 'q' -> Tuint16
  | 'i' -> Tint32
  | 'u' -> Tuint32
  | 'x' -> Tint64
  | 't' -> Tuint64
  | 'd' -> Tdouble
  | 's' -> Tstring
  | 'o' -> Tobject_path
  | 'g' -> Tsignature
  | c -> raise (Failure (Printf.sprintf "unknown type code %c" c))

let rec read_dtype str i =
  match str.[i] with
    | 'a' ->
        if str.[i + 1] = '{'
        then begin
          let tkey = basic_of_char str.[i + 2] in
          let i, tval = read_dtype str (i + 3) in
            if str.[i] <> '}'
            then raise (Failure "'}' expected")
            else (i + 1, Tdict(tkey, tval))
        end else begin
          let i, t = read_dtype str (i + 1) in
            (i, Tarray(t))
        end
    | '(' ->
        let i, t = read_until str ')' (i + 1) in
          (i, Tstructure(t))
    | 'v' -> (i + 1, Tvariant)
    | c -> (i + 1, basic_of_char c)

and read_until str cend i =
  if str.[i] = cend
  then (i + 1, [])
  else
    let i, hd = read_dtype str i in
    let i, tl = read_until str cend i in
      (i, hd :: tl)

let rec read_dtypes str limit i =
  if i = limit
  then []
  else
    let i, hd = read_dtype str i in
      hd :: read_dtypes str limit i

let dtype_of_signature signature = snd (read_dtype signature 0)
let dtypes_of_signature signature = read_dtypes signature (String.length signature) 0
