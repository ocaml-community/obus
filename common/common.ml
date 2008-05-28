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

let rec _dtype_signature_size acc = function
  | Tarray(t) -> _dtype_signature_size (acc + 1) t
  | Tdict(_, t) -> _dtype_signature_size (acc + 4) t
  | Tstructure ts-> _dtypes_signature_size (acc + 2) ts
  | _ -> acc + 1
and _dtypes_signature_size acc ts = List.fold_left _dtype_signature_size acc ts

let dtype_signature_size = _dtype_signature_size 0
let dtypes_signature_size = _dtypes_signature_size 0

let rec _write_dtype buffer i = function
  | Tbyte -> String.unsafe_set buffer i 'y'; i + 1
  | Tboolean -> String.unsafe_set buffer i 'b'; i + 1
  | Tint16 -> String.unsafe_set buffer i 'n'; i + 1
  | Tuint16 -> String.unsafe_set buffer i 'q'; i + 1
  | Tint32 -> String.unsafe_set buffer i 'i'; i + 1
  | Tuint32 -> String.unsafe_set buffer i 'u'; i + 1
  | Tint64 -> String.unsafe_set buffer i 'x'; i + 1
  | Tuint64 -> String.unsafe_set buffer i 't'; i + 1
  | Tdouble -> String.unsafe_set buffer i 'd'; i + 1
  | Tstring -> String.unsafe_set buffer i 's'; i + 1
  | Tobject_path -> String.unsafe_set buffer i 'o'; i + 1
  | Tsignature -> String.unsafe_set buffer i 'g'; i + 1
  | Tarray(t) ->
      String.unsafe_set buffer i 'a';
      _write_dtype buffer (i + 1) t
  | Tdict(tk, tv) ->
      String.unsafe_set buffer i 'a';
      String.unsafe_set buffer (i + 1) '{';
      let i = _write_dtype buffer (i + 2) tk in
      let i = _write_dtype buffer i tv in
        String.unsafe_set buffer i '}';
        i + 1
  | Tstructure(ts) ->
      String.unsafe_set buffer i '(';
      let i = _write_dtypes buffer (i + 1) ts in
        String.unsafe_set buffer i ')';
        i + 1
  | Tvariant ->  String.unsafe_set buffer i 'v'; i + 1
and _write_dtypes buffer = List.fold_left (_write_dtype buffer)

let write_dtype buffer i t = String.unsafe_set buffer (_write_dtype buffer i t) '\x00'
let write_dtypes buffer i ts = String.unsafe_set buffer (_write_dtypes buffer i ts) '\x00'

let signature_of_dtype t =
  let len = dtype_signature_size t in
  let str = String.create len in
    ignore (_write_dtype str 0 t);
    str

let signature_of_dtypes ts =
  let len = dtypes_signature_size ts in
  let str = String.create len in
    ignore (_write_dtypes str 0 ts);
    str

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

let rec _read_dtype buffer i =
  match String.unsafe_get buffer i with
    | 'a' ->
        if String.unsafe_get buffer (i + 1) = '{'
        then begin
          let tk = basic_of_char (String.unsafe_get buffer (i + 2)) in
          let i, tv = _read_dtype buffer (i + 3) in
            if String.unsafe_get buffer i <> '}'
            then raise (Failure "'}' expected")
            else (i + 1, Tdict(tk, tv))
        end else begin
          let i, t = _read_dtype buffer (i + 1) in
            (i, Tarray(t))
        end
    | '(' ->
        let i, t = read_until ')' buffer (i + 1) in
          (i, Tstructure(t))
    | 'v' -> (i + 1, Tvariant)
    | c -> (i + 1, basic_of_char c)

and read_until cend buffer i =
  if String.unsafe_get buffer i = cend
  then (i + 1, [])
  else
    let i, hd = _read_dtype buffer i in
    let i, tl = read_until cend buffer i in
      (i, hd :: tl)

let read_dtype buffer i =
  let i, t = _read_dtype buffer i in
    if String.unsafe_get buffer i = '\x00'
    then t
    else raise (Failure "null char missing after signature of single type")
let read_dtypes buffer i = snd (read_until '\x00' buffer i)

let dtype_of_signature signature = read_dtype (signature ^ "\x00") 0
let dtypes_of_signature signature = read_dtypes (signature ^ "\x00") 0
