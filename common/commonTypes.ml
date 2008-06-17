(*
 * types.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module Types =
struct
  type basic =
      [ `byte
      | `boolean
      | `int16
      | `int32
      | `int64
      | `uint16
      | `uint32
      | `uint64
      | `double
      | `string
      | `signature
      | `object_path ]
  type t =
      [ basic
      | `array of t
      | `dict of basic * t
      | `structure of t list
      | `variant ]
end

include Types
type signature = string

open WireTypes

module WireParams =
struct
  let set = String.unsafe_set
  let get str i =
    if i > String.length str
    then fail i "unterminated signature"
    else String.unsafe_get str i
  let terminated str i = i = String.length str
end

include Make(Types)(WireParams)

let to_signature ts =
  let len = signature_size ts in
  let str = String.create len in
    ignore (write str 0 ts);
    str

open Printf

let of_signature signature =
  try
    snd (read signature 0)
  with
      Fail(i, msg) ->
        raise (Invalid_argument
                 (sprintf "invalid signature %S, at position %d: %s" signature i msg))

let to_string t =
  let aux_basic = function
    | `byte -> "byte"
    | `boolean -> "boolean"
    | `int16 -> "int16"
    | `int32 -> "int32"
    | `int64 -> "int64"
    | `uint16 -> "uint16"
    | `uint32 -> "uint32"
    | `uint64 -> "uint64"
    | `double -> "double"
    | `string -> "string"
    | `signature -> "signature"
    | `object_path -> "object_path"
  in
  let rec aux top = function
    | #basic as t -> aux_basic t
    | `array t -> sprintf "%s array" (aux false t)
    | `dict(tk, tv) -> sprintf "(%s, %s) dict" (aux_basic tk) (aux true tv)
    | `structure tl ->
        let s = String.concat " * " (List.map (aux false) tl) in
          if top then s else sprintf "(%s)" s
    | `variant -> "variant"
  in
    aux true t
