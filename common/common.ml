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
