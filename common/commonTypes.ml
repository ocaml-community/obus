(*
 * types.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Common function for reading/writing signatures of dbus types *)

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

type aligned_on_8_boundary =
    [ `int64 | `uint64 | `double | `structure of t list ]

type signature = string

module type WireParams =
sig
  (* These parameters depends on if we are reading/writing a
     string containg only one signature or if the signature is part of a
     message *)
  val set : string -> int -> char -> unit
  val get : string -> int -> char
  val terminated : string -> int -> bool
end

exception Parse_failure of int(* position *) * string(* reason *)

let fail i fmt = Printf.ksprintf (fun s -> raise (Parse_failure(i, s))) fmt

module MakeWire(Params : WireParams) =
struct
  open Params

  let char_of_basic = function
    | `byte -> 'y'
    | `boolean -> 'b'
    | `int16 -> 'n'
    | `uint16 -> 'q'
    | `int32 -> 'i'
    | `uint32 -> 'u'
    | `int64 -> 'x'
    | `uint64 -> 't'
    | `double -> 'd'
    | `string -> 's'
    | `object_path -> 'o'
    | `signature -> 'g'

  let rec write_single buffer i = function
    | #basic as t ->
        set buffer i (char_of_basic t);
        i + 1
    | `array(t) ->
        set buffer i 'a';
        write_single buffer (i + 1) t
    | `dict(tk, tv) ->
        set buffer i 'a';
        set buffer (i + 1) '{';
        set buffer (i + 2) (char_of_basic tk);
        let i = write_single buffer (i + 3) tv in
          set buffer i '}';
          i + 1
    | `structure(ts) ->
        set buffer i '(';
        let i = write buffer (i + 1) ts in
          set buffer i ')';
          i + 1
    | `variant ->  set buffer i 'v'; i + 1

  and write buffer = List.fold_left (write_single buffer)

  let basic_of_char on_fail = function
    | 'y' -> `byte
    | 'b' -> `boolean
    | 'n' -> `int16
    | 'q' -> `uint16
    | 'i' -> `int32
    | 'u' -> `uint32
    | 'x' -> `int64
    | 't' -> `uint64
    | 'd' -> `double
    | 's' -> `string
    | 'o' -> `object_path
    | 'g' -> `signature
    | chr -> on_fail chr

  let read_dict_key_type buffer i =
    basic_of_char
      (fun chr -> fail i "invalid basic type code: %c" chr)
      (get buffer i)

  let rec read_single buffer i =
    match get buffer i with
      | 'a' ->
          if get buffer (i + 1) = '{'
          then begin
            let tk = read_dict_key_type buffer (i + 2) in
            let i, tv = read_single buffer (i + 3) in
              if get buffer i <> '}'
              then fail i "'}' missing"
              else (i + 1, `dict(tk, tv))
          end else begin
            let i, t = read_single buffer (i + 1) in
              (i, `array(t))
          end
      | '(' ->
          let i, t = read_until (fun buffer i -> get buffer i = ')') buffer (i + 1) in
            (i, `structure(t))
      | 'v' -> (i + 1, `variant)
      | ch ->
          (i + 1,
           basic_of_char (fun chr -> fail i "invalid type code: %c" chr) ch)

  and read_until f buffer i =
    if f buffer i
    then (i + 1, [])
    else
      let i, hd = read_single buffer i in
      let i, tl = read_until f buffer i in
        (i, hd :: tl)

  let read buffer i = read_until terminated buffer i
end

(* Parameters for a string containing only a signature, both the
   library library and the binding tools use it. *)
module WireParams =
struct
  (* The signature size is calculated before writing effectively the
     signature, so this is safe: *)
  let set = String.unsafe_set

  let get str i =
    if i > String.length str
    then fail i "unterminated signature"
    else String.unsafe_get str i
  let terminated str i = i = String.length str
end

include MakeWire(WireParams)

let signature_size tl =
  let rec aux acc = function
    | `array t -> aux (acc + 1) t
    | `dict(_, t) -> aux (acc + 4) t
    | `structure tl -> List.fold_left aux (acc + 2) tl
    | _ -> acc + 1
  in
    List.fold_left aux 0 tl

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
      Parse_failure(i, msg) ->
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
