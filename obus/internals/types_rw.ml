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
    | Tstructure of tsingle list
    | Tarray of telement
    | Tvariant
  and telement =
    | Tdict_entry of tbasic * tsingle
    | Tsingle of tsingle
  type tsequence = tsingle list
end

module type Char_reader = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val failwith : string -> 'a t

  val get_char : char t
  val eof : bool t
end

module type Char_writer = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val put_char : char -> unit t
end

module Make_reader(Types : Types)(Reader : Char_reader) : sig

  val read : Types.tsequence Reader.t
    (** Parse a signature *)

end = struct
  open Types
  open Reader

  let ( >>= ) = bind

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

  let rec parse_single = function
    | 'a' ->
        (perform
           t <-- get_char >>= parse_element;
           return (Tarray t))
    | '(' ->
        (perform
           t <-- get_char >>= parse_struct;
           return (Tstructure t))
    | ')' ->
        failwith "')' without '('"
    | 'v' ->
        return Tvariant
    | ch ->
        (perform
           t <-- parse_basic "invalid type code: %c" ch;
           return (Tbasic t))

  and parse_struct = function
    | ')' ->
        return []
    | ch ->
        (perform
           t <-- parse_single ch;
           l <-- get_char >>= parse_struct;
           return (t :: l))

  and parse_element = function
    | '{' ->
        (perform
           tk <-- get_char >>= parse_basic "invalid basic type code: %c";
           tv <-- get_char >>= parse_single;
           get_char >>= function
             | '}' -> return (Tdict_entry(tk, tv))
             | _ -> failwith "'}' missing")
    | ch ->
        (perform
           t <-- parse_single ch;
           return (Tsingle t))

  let rec parse_sequence = function
    | true ->
        return []
    | false ->
        (perform
           t <-- get_char >>= parse_single;
           l <-- eof >>= parse_sequence;
           return (t :: l))

  let read = eof >>= parse_sequence
end

module Make_writer(Types : Types)(Writer : Char_writer) : sig
  val char_of_basic : Types.tbasic -> char
    (** Returns the type code of a basic type *)

  val write : Types.tsequence -> int * unit Writer.t
    (** Returns the length of a marshaled signature and a writer *)
end = struct
  open Types
  open Writer

  let ( >>= ) = bind

  let rec single_size_aux acc = function
    | Tarray t -> begin match t with
        | Tdict_entry(_, t) -> single_size_aux (acc + 4) t
        | Tsingle t -> single_size_aux (acc + 1) t
      end
    | Tstructure tl -> List.fold_left single_size_aux (acc + 2) tl
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

  let rec write_single = function
    | Tbasic t ->
        (1, put_char (char_of_basic t))
    | Tarray t ->
        let sz, wr = write_element t in
        (sz + 1, perform put_char 'a'; wr)
    | Tstructure ts ->
        let sz, wr = write_sequence ts in
        (sz + 2, perform put_char '('; wr; put_char ')')
    | Tvariant ->
        (1, put_char 'v')

  and write_element = function
    | Tdict_entry(tk, tv) ->
        let sz, wr = write_single tv in
        (sz + 3,
         perform
           put_char '{';
           put_char (char_of_basic tk);
           wr;
           put_char '}')
    | Tsingle t ->
        write_single t

  and write_sequence = function
    | [] ->
        0, return ()
    | x :: l ->
        let shd, whd = write_single x
        and stl, wtl = write_sequence l in
        (shd + stl, perform whd; wtl)

  let write = write_sequence
end
