(*
 * common.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let max_array_size = 1 lsl 26

module type TypesSig = sig
  type basic =
    | Byte
    | Boolean
    | Int16
    | Int32
    | Int64
    | Uint16
    | Uint32
    | Uint64
    | Double
    | String
    | Signature
    | Object_path
  type single =
    | Basic of basic
    | Array of single
    | Dict of basic * single
    | Structure of single list
    | Variant
  type t = single list
end

module type Exn = sig
  exception Fail of string
end

module Signature(T : TypesSig)(E : Exn) =
struct
  let basic_of_char = function
    | 'y' -> T.Byte
    | 'b' -> T.Boolean
    | 'n' -> T.Int16
    | 'q' -> T.Uint16
    | 'i' -> T.Int32
    | 'u' -> T.Uint32
    | 'x' -> T.Int64
    | 't' -> T.Uint64
    | 'd' -> T.Double
    | 's' -> T.String
    | 'o' -> T.Object_path
    | 'g' -> T.Signature
    | _ -> raise (E.Fail (Printf.sprintf "unknown basic type code %c" c))

  let rec read_single str i =
    match str.[i] with
      | 'y' -> (i + 1, T.Basic T.Byte)
      | 'b' -> (i + 1, T.Basic T.Boolean)
      | 'n' -> (i + 1, T.Basic T.Int16)
      | 'q' -> (i + 1, T.Basic T.Uint16)
      | 'i' -> (i + 1, T.Basic T.Int32)
      | 'u' -> (i + 1, T.Basic T.Uint32)
      | 'x' -> (i + 1, T.Basic T.Int64)
      | 't' -> (i + 1, T.Basic T.Uint64)
      | 'd' -> (i + 1, T.Basic T.Double)
      | 's' -> (i + 1, T.Basic T.String)
      | 'o' -> (i + 1, T.Basic T.Object_path)
      | 'g' -> (i + 1, T.Basic T.Signature)
      | 'a' ->
          if str.[i + 1] = '{'
          then begin
            let tkey = basic_of_char str.[i + 2] in
            let i, tval = read_single str (i + 3) in
              if str.[i] <> '}'
              then raise (E.Fail "'}' expected")
              else (i + 1, T.Dict(tkey, tval))
          end else begin
            let i, t = read_single i in
              (i, T.Array(t))
          end
      | '(' ->
          let i, t = read_single_until str ')' i in
            (i, T.Structure(t))
      | 'v' -> (i + 1, T.Variant)
      | _ -> raise (E.Fail (Printf.sprintf "unknown type code %c" c))

  and read_single_type_until str cend i =
    if str.[i] = cend
    then (i + 1, [])
    else
      let i, hd = read_single i in
      let i, tl = read_single_until str cend i in
        (i, hd :: tl)

  let read_type str = read_single_until str '\x00'

  let char_of_basic = function
    | T.Byte -> 'y'
    | T.Boolean -> 'b'
    | T.Int16 -> 'n'
    | T.Uint16 -> 'q'
    | T.Int32 -> 'i'
    | T.Uint32 -> 'u'
    | T.Int64 -> 'x'
    | T.Uint64 -> 't'
    | T.Double -> 'd'
    | T.String -> 's'
    | T.Object_path -> 'o'
    | T.Signature -> 'g'

  let rec write_single str i = function
    | T.Basic(t) ->
        str.[i] <- char_of_basic t;
        i + 1
    | T.Array(t) ->
        str.[i] <- 'a';
        write_single str (i + 1) t
    | T.Dict(tk, tv) ->
        str.[i] <- 'a';
        str.[i + 1] <- '{';
        str.[i + 2] <- char_of_basic tk;
        let i = write_single str (i + 3) tv in
          str.[i] <- '}';
          i + 1
    | T.Structure(t) ->
        str.[i] <- '(';
        let i = List.fold_left (write_single str) i t in
          str.[i] <- ')';
          i + 1
    | T.Variant ->
        str.[i] <- 'v';
        i + 1

  let write_t str i t = List.fold_left (write_single str) i t
end
