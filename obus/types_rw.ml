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
end

module type Reader_params =
sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val failwith : ('b, unit, string, 'a t) format4 -> 'b

  type input
  val get : input -> char t
    (* get must fail on end-of-input *)
  val get_opt : input -> char option t
    (* get_opt must not fail on end-of-input *)
end

module Make_reader(Types : Types)(Params : Reader_params) =
struct
  open Types
  open Params

  let (>>=) = bind

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
    | chr -> failwith msg chr

  let rec parse_single ic = function
    | 'a' ->
        (perform
           t <-- read_element ic;
           return (Tarray t))
    | '(' ->
        (perform
           t <-- read_struct ic;
           return (Tstruct t))
    | ')' ->
        failwith "')' without '('"
    | 'v' ->
        return Tvariant
    | ch ->
        (perform
           t <-- parse_basic "invalid type code: %c" ch;
           return (Tbasic t))

  and read_struct ic = get ic >>= function
    | ')' ->
        return []
    | ch ->
        (perform
           t <-- parse_single ic ch;
           l <-- read_struct ic;
           return (t :: l))

  and read_element ic = get ic >>= function
    | '{' ->
        (perform
           ch <-- get ic;
           tk <-- parse_basic "invalid basic type code: %c" ch;
           tv <-- get ic >>= parse_single ic;
           get ic >>= function
             | '}' -> return (Tdict_entry(tk, tv))
             | _ -> failwith "'}' missing")
    | ch ->
        (perform
           t <-- parse_single ic ch;
           return (Tsingle t))

  and read_sequence ic = get_opt ic >>= function
    | None ->
        return []
    | Some ch ->
        (perform
           t <-- parse_single ic ch;
           l <-- read_sequence ic;
           return (t :: l))
end

module type Writer_params =
sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  type output
  val put : output -> char -> unit t
end

module Make_writer(Types : Types)(Params : Writer_params) =
struct
  open Types
  open Params

  let rec single_signature_size_aux acc = function
    | Tarray t -> begin match t with
        | Tdict_entry(_, t) -> single_signature_size_aux (acc + 4) t
        | Tsingle t -> single_signature_size_aux (acc + 1) t
      end
    | Tstruct tl -> List.fold_left single_signature_size_aux (acc + 2) tl
    | _ -> acc + 1

  let single_signature_size = single_signature_size_aux 0

  let signature_size = List.fold_left single_signature_size_aux 0

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

  let rec write_single oc = function
    | Tbasic t ->
        put oc (char_of_basic t);
    | Tarray t ->
        (perform
           put oc 'a';
           write_element oc t)
    | Tstruct ts ->
        (perform
           put oc '(';
           write_sequence oc ts;
           put oc ')')
    | Tvariant ->
        put oc 'v';

  and write_element oc = function
    | Tdict_entry(tk, tv) ->
        (perform
           put oc '{';
           put oc (char_of_basic tk);
           write_single oc tv;
           put oc '}')
    | Tsingle t ->
        write_single oc t

  and write_sequence oc = function
    | [] ->
        return ()
    | x :: l ->
        (perform
           write_single oc x;
           write_sequence oc l)
end
