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
  type tbasic =
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
  type tsingle =
    | Tbasic of tbasic
    | Tstruct of tsingle list
    | Tarray of telement
    | Tvariant
  and telement =
    | Tdict_entry of tbasic * tsingle
    | Tsingle of tsingle
  type tsequence = tsingle list
end

module Make(Types : Types)(Monad : OBus_monad.S) : sig
  val single_size : Types.tsingle -> int
  val sequence_size : Types.tsequence -> int
    (** Returns the marshaled length of a signature *)

  val read_single : (unit -> char option Monad.t) -> Types.tsingle Monad.t
  val read_sequence : (unit -> char option Monad.t) -> Types.tsequence Monad.t
    (** Parse a signature using the given get_char function *)

  val write_single : (char -> unit Monad.t) -> Types.tsingle -> unit Monad.t
  val write_sequence : (char -> unit Monad.t) -> Types.tsequence -> unit Monad.t
    (** Marshal a signature using the given put_char function *)

  val char_of_basic : Types.tbasic -> char
    (** Returns the type code of a basic type *)
end = struct
  open Types
  open Monad

  let ( >>= ) = bind

  let read_char get_char = get_char () >>= function
    | Some ch -> return ch
    | None -> failwith "premature end of signature"

  let parse_basic msg = function
    | 'y' -> return Tbyte
    | 'b' -> return Tboolean
    | 'n' -> return Tint16
    | 'q' -> return Tuint16
    | 'i' -> return Tint32
    | 'u' -> return Tuint32
    | 'x' -> return Tint64
    | 't' -> return Tuint64
    | 'd' -> return Tdouble
    | 's' -> return Tstring
    | 'o' -> return Tobject_path
    | 'g' -> return Tsignature
    | chr -> Printf.ksprintf failwith msg chr

  let rec parse_single get_char = function
    | 'a' ->
        (perform
           t <-- read_element get_char;
           return (Tarray t))
    | '(' ->
        (perform
           t <-- read_struct get_char;
           return (Tstruct t))
    | ')' ->
        failwith "')' without '('"
    | 'v' ->
        return Tvariant
    | ch ->
        (perform
           t <-- parse_basic "invalid type code: %c" ch;
           return (Tbasic t))

  and read_single get_char = read_char get_char >>= parse_single get_char

  and read_struct get_char = read_char get_char >>= function
    | ')' ->
        return []
    | ch ->
        (perform
           t <-- parse_single get_char ch;
           l <-- read_struct get_char;
           return (t :: l))

  and read_element get_char = read_char get_char >>= function
    | '{' ->
        (perform
           ch <-- read_char get_char;
           tk <-- parse_basic "invalid basic type code: %c" ch;
           tv <-- read_char get_char >>= parse_single get_char;
           read_char get_char >>= function
             | '}' -> return (Tdict_entry(tk, tv))
             | _ -> failwith "'}' missing")
    | ch ->
        (perform
           t <-- parse_single get_char ch;
           return (Tsingle t))

  and read_sequence get_char = get_char () >>= function
    | None ->
        return []
    | Some ch ->
        (perform
           t <-- parse_single get_char ch;
           l <-- read_sequence get_char;
           return (t :: l))

  let rec single_size_aux acc = function
    | Tarray t -> begin match t with
        | Tdict_entry(_, t) -> single_size_aux (acc + 4) t
        | Tsingle t -> single_size_aux (acc + 1) t
      end
    | Tstruct tl -> List.fold_left single_size_aux (acc + 2) tl
    | _ -> acc + 1

  let single_size = single_size_aux 0

  let sequence_size = List.fold_left single_size_aux 0

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

  let rec write_single put_char = function
    | Tbasic t ->
        put_char (char_of_basic t);
    | Tarray t ->
        (perform
           put_char 'a';
           write_element put_char t)
    | Tstruct ts ->
        (perform
           put_char '(';
           write_sequence put_char ts;
           put_char ')')
    | Tvariant ->
        put_char 'v';

  and write_element put_char = function
    | Tdict_entry(tk, tv) ->
        (perform
           put_char '{';
           put_char (char_of_basic tk);
           write_single put_char tv;
           put_char '}')
    | Tsingle t ->
        write_single put_char t

  and write_sequence put_char = function
    | [] ->
        return ()
    | x :: l ->
        (perform
           write_single put_char x;
           write_sequence put_char l)
end
