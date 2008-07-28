(*
 * types_rw.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Types printer/parser *)

module type Types = sig
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

module type Parser_params =
sig
  (* These parameters depends on if we are reading/writing a string
     containg only one signature or if the signature is part of a
     message *)
  val get : string -> int -> char
  val terminated : string -> int -> bool
end

exception Parse_failure of int(* position *) * string(* reason *)

let fail i fmt = Printf.ksprintf (fun s -> raise (Parse_failure(i, s))) fmt

module Make_reader(Types : Types)(Params : Parser_params) =
struct
  open Types
  open Params

  let basic_of_char on_fail = function
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
              else (i + 1, Tdict(tk, tv))
          end else begin
            let i, t = read_single buffer (i + 1) in
              (i, Tarray t)
          end
      | '(' ->
          let i, t = read_until (fun buffer i -> get buffer i = ')') buffer (i + 1) in
            (i, Tstruct t)
      | 'v' -> (i + 1, Tvariant)
      | ch ->
          (i + 1,
           Tbasic (basic_of_char (fun chr -> fail i "invalid type code: %c" chr) ch))

  and read_until f buffer i =
    if f buffer i
    then (i + 1, [])
    else
      let i, hd = read_single buffer i in
      let i, tl = read_until f buffer i in
        (i, hd :: tl)

  let read_sequence buffer i = read_until terminated buffer i
end

module Make_writer(Types : Types) =
struct
  open Types

  let signature_size tl =
    let rec aux acc = function
      | Tarray t -> aux (acc + 1) t
      | Tdict(_, t) -> aux (acc + 4) t
      | Tstruct tl -> List.fold_left aux (acc + 2) tl
      | _ -> acc + 1
    in
      List.fold_left aux 0 tl

  let char_of_basic = function
    | Tbyte -> 'y'
    | Tboolean -> 'b'
    | Tint16 -> 'n'
    | Tuint16 -> 'q'
    | Tint32 -> 'i'
    | Tuint32 -> 'u'
    | Tint64 -> 'x'
    | Tuint64 -> 't'
    | Tdouble -> 'd'
    | Tstring -> 's'
    | Tobject_path -> 'o'
    | Tsignature -> 'g'

  let rec write_single buffer i = function
    | Tbasic t ->
        String.unsafe_set buffer i (char_of_basic t);
        i + 1
    | Tarray t ->
        String.unsafe_set buffer i 'a';
        write_single buffer (i + 1) t
    | Tdict(tk, tv) ->
        String.unsafe_set buffer i 'a';
        String.unsafe_set buffer (i + 1) '{';
        String.unsafe_set buffer (i + 2) (char_of_basic tk);
        let i = write_single buffer (i + 3) tv in
          String.unsafe_set buffer i '}';
          i + 1
    | Tstruct ts ->
        String.unsafe_set buffer i '(';
        let i = write_sequence buffer (i + 1) ts in
          String.unsafe_set buffer i ')';
          i + 1
    | Tvariant ->  String.unsafe_set buffer i 'v'; i + 1

  and write_sequence buffer = List.fold_left (write_single buffer)
end
