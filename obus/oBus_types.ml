(*
 * oBus_types.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf

module T =
struct
  type basic =
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
  type single =
    | Tbasic of basic
    | Tstruct of single list
    | Tarray of single
    | Tdict of basic * single
    | Tvariant
end

include T

type sequence = single list
type signature = sequence

let string_of_basic = function
  | Tbyte -> "byte"
  | Tboolean -> "boolean"
  | Tint16 -> "int16"
  | Tint32 -> "int32"
  | Tint64 -> "int64"
  | Tuint16 -> "uint16"
  | Tuint32 -> "uint32"
  | Tuint64 -> "uint64"
  | Tdouble -> "double"
  | Tstring -> "string"
  | Tsignature -> "signature"
  | Tobject_path -> "object_path"

let rec string_of_single = function
  | Tbasic t -> string_of_basic t
  | Tarray t -> sprintf "%s array" (string_of_single t)
  | Tdict(k, v) -> sprintf "(%s, %s) dict" (string_of_basic k) (string_of_single v)
  | Tstruct tl -> sprintf "(%s) structure" (string_of_sequence tl)
  | Tvariant -> "variant"

and string_of_sequence tl = String.concat " * " (List.map string_of_single tl)

open Types_rw

module Parser_params =
struct
  let get str i =
    if i > String.length str
    then fail i "unterminated signature"
    else String.unsafe_get str i

  let terminated str i = i = String.length str
end

module R = Make_reader(T)(Parser_params)
module W = Make_writer(T)

let string_of_signature ts =
  let len = W.signature_size ts in
  let str = String.create len in
    ignore (W.unsafe_write_sequence str 0 ts);
    str

let signature_of_string signature =
  try
    snd (R.read_sequence signature 0)
  with
      Parse_failure(i, msg) ->
        raise (Invalid_argument
                 (sprintf "signature_of_string: invalid signature %S, at position %d: %s" signature i msg))
