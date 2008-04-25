(*
 * common.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module type TypesSig = sig
  type typ =
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
    | Tbasic of typ
    | Tarray of typ
    | Tdict of typ * typ
    | Tstructure of typ list
    | Tvariant
end

module type Exn = sig
  exception Fail of string
end

module Signature(T : TypesSig)(E : Exn) =
struct
  open T

  let rec read str i =
    match str.[i] with
      | 'y' -> (i + 1, Tbyte)
      | 'b' -> (i + 1, Tboolean)
      | 'n' -> (i + 1, Tint16)
      | 'q' -> (i + 1, Tuint16)
      | 'i' -> (i + 1, Tint32)
      | 'u' -> (i + 1, Tuint32)
      | 'x' -> (i + 1, Tint64)
      | 't' -> (i + 1, Tuint64)
      | 'd' -> (i + 1, Tdouble)
      | 's' -> (i + 1, Tstring)
      | 'o' -> (i + 1, Tobject_path)
      | 'g' -> (i + 1, Tsignature)
      | 'a' ->
          if str.[i + 1] = '{'
          then begin
            let tkey = basic_of_char str.[i + 2] in
            let i, tval = read_single str (i + 3) in
              if str.[i] <> '}'
              then raise (E.Fail "'}' expected")
              else (i + 1, Tdict(tkey, tval))
          end else begin
            let i, t = read_single i in
              (i, Tarray(t))
          end
      | '(' ->
          let i, t = read_until str ')' i in
            (i, Tstructure(t))
      | 'v' -> (i + 1, Tvariant)
      | _ -> raise (E.Fail (Printf.sprintf "unknown type code %c" c))

  and read_until str cend i =
    if str.[i] = cend
    then (i + 1, [])
    else
      let i, hd = read i in
      let i, tl = read_until str cend i in
        (i, hd :: tl)

  let read_list str = read_until str '\x00'

  let rec write str i = function
    | Tbyte -> str.[i] <- 'y'; i + 1
    | Tboolean -> str.[i] <- 'b'; i + 1
    | Tint16 -> str.[i] <- 'n'; i + 1
    | Tuint16 -> str.[i] <- 'q'; i + 1
    | Tint32 -> str.[i] <- 'i'; i + 1
    | Tuint32 -> str.[i] <- 'u'; i + 1
    | Tint64 -> str.[i] <- 'x'; i + 1
    | Tuint64 -> str.[i] <- 't'; i + 1
    | Tdouble -> str.[i] <- 'd'; i + 1
    | Tstring -> str.[i] <- 's'; i + 1
    | Tobject_path -> str.[i] <- 'o'; i + 1
    | Tsignature -> str.[i] <- 'g'; i + 1
    | Tarray(t) ->
        str.[i] <- 'a';
        write str (i + 1) t
    | Tdict(tk, tv) ->
        str.[i] <- 'a';
        str.[i + 1] <- '{';
        str.[i + 2] <- char_of_basic tk;
        let i = write str (i + 3) tv in
          str.[i] <- '}';
          i + 1
    | Tstructure(t) ->
        str.[i] <- '(';
        let i = List.fold_left (write str) i t in
          str.[i] <- ')';
          i + 1
    | Tvariant ->
        str.[i] <- 'v';
        i + 1

  let write_list str i t = List.fold_left (write str) i t
end
