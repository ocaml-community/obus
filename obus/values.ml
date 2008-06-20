(*
 * values.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type t =
  | Byte of char
  | Boolean of bool
  | Int16 of int
  | Int32 of int32
  | Int64 of int64
  | Uint16 of int
  | Uint32 of int32
  | Uint64 of int64
  | Double of float
  | String of string
  | Signature of Types.t list
  | Object_path of Path.t
  | Array of Types.t * t list
  | Dict of Types.basic  * Types.t * (t * t) list
  | Structure of t list
  | Variant of t

let rec typ = function
  | Byte _ -> `byte
  | Boolean _ -> `boolean
  | Int16 _ -> `int16
  | Int32 _ -> `int32
  | Int64 _ -> `int64
  | Uint16 _ -> `uint16
  | Uint32 _ -> `uint32
  | Uint64 _ -> `uint64
  | Double _ -> `double
  | String _ -> `string
  | Signature _ -> `signature
  | Object_path _ -> `object_path
  | Array(t, _) -> `array(t)
  | Dict(tk, tv, _) -> `dict(tk, tv)
  | Structure(l) -> `structure(List.map typ l)
  | Variant _ -> `variant

open Printf

let to_string v =
  let seq sep f l = String.concat sep (List.map f l) in
  let rec aux = function
    | Byte x -> sprintf "%C" x
    | Boolean x -> string_of_bool x
    | Int16 x -> string_of_int x
    | Int32 x -> Int32.to_string x
    | Int64 x -> Int64.to_string x
    | Uint16 x -> string_of_int x
    | Uint32 x -> Int32.to_string x
    | Uint64 x -> Int64.to_string x
    | Double x -> string_of_float x
    | String x -> sprintf "%S" x
    | Signature x -> sprintf "T<%s>" (Types.to_string (`structure x))
    | Object_path x -> sprintf "%S" x
    | Array(t, xl) -> sprintf "[%s]" (seq "; " aux xl)
    | Dict(tk, tv, xl) ->
        sprintf "[%s]"
          (seq "; " (fun (a, b) -> sprintf "(%s, %s)" (aux a) (aux b)) xl)
    | Structure xl -> sprintf "(%s)" (seq ", " aux xl)
    | Variant x -> sprintf "V<%s>" (aux x)
  in
    aux v

let byte x = Byte x
let boolean x = Boolean x
let int16 x = Int16 x
let int32 x = Int32 x
let int64 x = Int64 x
let uint16 x = Uint16 x
let uint32 x = Uint32 x
let uint64 x = Uint64 x
let double x = Double x
let string x = String x
let signature x = Signature x
let object_path x = Object_path x
let array t x =
  List.iter (fun v ->
               if typ v <> t then
                 raise (Invalid_argument "element of arrays must all have the same type")) x;
  Array(t, x)
let dict tk tv x =
  List.iter (fun (k, v) ->
               if typ k <> (tk : Types.basic :> Types.t)
                 || typ v <> tv then
                   raise (Invalid_argument "element of dictionnaries must all have the same type")) x;
  Dict(tk, tv, x)
let structure x = Structure x
let variant x = Variant x

module Safe =
struct
  type 'a basic = Types.basic * ('a -> t) * (t -> 'a)
  type 'a single = Types.t * ('a -> t) * (t -> 'a)
  type 'a seq = Types.t list * ('a -> t list) * (t list -> 'a)

  let invalid_type () = raise (Failure "invalid type")

  let byte = (`byte,
              (fun x -> Byte x),
              (function
                 | Byte x -> x
                 | _ -> invalid_type ()))
  let boolean = (`boolean,
                 (fun x -> Boolean x),
                 (function
                    | Boolean x -> x
                    | _ -> invalid_type ()))
  let int16 = (`int16,
               (fun x -> Int16 x),
               (function
                  | Int16 x -> x
                  | _ -> invalid_type ()))
  let int32 = (`int32,
               (fun x -> Int32 x),
               (function
                  | Int32 x -> x
                  | _ -> invalid_type ()))
  let int64 = (`int64,
               (fun x -> Int64 x),
               (function
                  | Int64 x -> x
                  | _ -> invalid_type ()))
  let uint16 = (`uint16,
                (fun x -> Uint16 x),
                (function
                   | Uint16 x -> x
                   | _ -> invalid_type ()))
  let uint32 = (`uint32,
                (fun x -> Uint32 x),
                (function
                   | Uint32 x -> x
                   | _ -> invalid_type ()))
  let uint64 = (`uint64,
                (fun x -> Uint64 x),
                (function
                   | Uint64 x -> x
                   | _ -> invalid_type ()))
  let double = (`double,
                (fun x -> Double x),
                (function
                   | Double x -> x
                   | _ -> invalid_type ()))
  let string = (`string,
                (fun x -> String x),
                (function
                   | String x -> x
                   | _ -> invalid_type ()))
  let signature = (`signature,
                   (fun x -> Signature x),
                   (function
                      | Signature x -> x
                      | _ -> invalid_type ()))
  let object_path = (`object_path,
                     (fun x -> Object_path x),
                     (function
                        | Object_path x -> x
                        | _ -> invalid_type ()))
  let basic (t, f, g) = ((t : Types.basic :> Types.t), f, g)
  let array (t, f, g) = (`array t,
                         (fun x -> Array (t, List.map f x)),
                         (function
                            | Array(_, x) -> List.map g x
                            | _ -> invalid_type ()))
  let dict (tk, fk, gk) (tv, fv, gv) = (`dict(tk, tv),
                                        (fun x -> Dict(tk, tv, List.map (fun (k, v) -> (fk k, fv v)) x)),
                                        (function
                                           | Dict(_, _, x) -> List.map (fun (k, v) -> (gk k, gv v)) x
                                           | _ -> invalid_type ()))
  let structure (t, f, g) = (`structure t,
                             (fun x -> Structure(f x)),
                             (function
                                | Structure x -> g x
                                | _ -> invalid_type ()))
  let variant = (`variant,
                 (fun x -> Variant x),
                 (function
                    | Variant x -> x
                    | _ -> invalid_type ()))
  let cons (t, f, g) (tl, fl, gl) = (t :: tl,
                                     (fun (x,y) -> f x :: fl y),
                                     (function
                                        | x :: y -> (g x, gl y)
                                        | _ -> invalid_type ()))
  let nil = ([],
             (fun () -> []),
             (function
                | [] -> ()
                | _ -> invalid_type ()))

  let make_single (_, f, _) = f
  let make = make_single
  let get_single (_, _, g) = g
  let get = get_single
end

open Wire

module type Reader = sig
  val read_variant : t reader
  val read : Types.t list -> t list reader
end

module type Writer = sig
  val write_variant : t writer
  val write : t list writer
end

module MakeReader(Reader : Wire.Reader) =
struct
  open Reader

  let read_basic t buffer i = match t with
    | `byte -> let i, v = read_char_byte buffer i in (i, Byte v)
    | `boolean -> let i, v = read_bool_boolean buffer i in (i, Boolean v)
    | `int16 -> let i, v = read_int_int16 buffer i in (i, Int16 v)
    | `int32 -> let i, v = read_int32_int32 buffer i in (i, Int32 v)
    | `int64 -> let i, v = read_int64_int64 buffer i in (i, Int64 v)
    | `uint16 -> let i, v = read_int_uint16 buffer i in (i, Uint16 v)
    | `uint32 -> let i, v = read_int32_uint32 buffer i in (i, Uint32 v)
    | `uint64 -> let i, v = read_int64_uint64 buffer i in (i, Uint64 v)
    | `double -> let i, v = read_float_double buffer i in (i, Double v)
    | `string -> let i, v = read_string_string buffer i in (i, String v)
    | `signature -> let i, v = read_types_signature buffer i in (i, Signature v)
    | `object_path -> let i, v = read_path_object_path buffer i in (i, Object_path v)

  let rec read_value t buffer i = match t with
    | #Types.basic as t -> read_basic t buffer i
    | `array t ->
        let i, v =
          (match t with
             | #CommonTypes.aligned_on_8_boundary -> read_array8
             | _ -> read_array) (fun limit buffer i ->
                                   read_until_rev
                                     (fun i cont ->
                                        let i, v = read_value t buffer i in
                                          v :: cont i)
                                     [] i limit) buffer i
        in
          (i, Array(t, v))
    | `dict(tk, tv) ->
        let i, v =
          read_array8 (fun limit buffer i ->
                         read_until
                           (fun i acc cont ->
                              let i = rpad8 i in
                              let i, k = read_basic tk buffer i in
                              let i, v = read_value tv buffer i in
                                cont i ((k, v) :: acc))
                           [] i limit) buffer i
        in
          (i, Dict(tk, tv, v))
    | `structure tl ->
        let i, v = read tl buffer (rpad8 i) in
          (i, Structure v)
    | `variant ->
        let i, v = read_variant buffer i in
          (i, Variant v)

  and read_variant buffer i =
    let i, tl = read_types_signature buffer i in
      match tl with
        | [t] -> read_value t buffer i
        | _ -> raise (Reading_error
                        (sprintf "invalid variant signature: %S, must contain exactly one single type"
                           (Types.to_signature tl)))

  and read tl buffer i = match tl with
    | [] -> (i, [])
    | t :: tl ->
        let i, v = read_value t buffer i in
        let i, vl = read tl buffer i in
          (i, v :: vl)
end

module MakeWriter(Writer : Wire.Writer) =
struct
  open Writer

  let rec write_value buffer i = function
    | Byte v -> write_char_byte buffer i v
    | Boolean v -> write_bool_boolean buffer i v
    | Int16 v -> write_int_int16 buffer i v
    | Int32 v -> write_int32_int32 buffer i v
    | Int64 v -> write_int64_int64 buffer i v
    | Uint16 v -> write_int_uint16 buffer i v
    | Uint32 v -> write_int32_uint32 buffer i v
    | Uint64 v -> write_int64_uint64 buffer i v
    | Double v -> write_float_double buffer i v
    | String v -> write_string_string buffer i v
    | Signature v -> write_types_signature buffer i v
    | Object_path v -> write_path_object_path buffer i v
    | Array(t, vs) ->
        (match t with
           | #CommonTypes.aligned_on_8_boundary -> write_array8
           | _ -> write_array)
          (fun buffer i vs -> List.fold_left (write_value buffer) i vs)
          buffer i vs
    | Dict(_, _, vs) ->
        write_array8
          (fun buffer i vs ->
             List.fold_left
               (fun i (k, v) ->
                  let i = wpad8 buffer i in
                  let i = write_value buffer i k in
                    write_value buffer i v) i vs)
          buffer i vs
    | Structure vs -> write buffer (wpad8 buffer i) vs
    | Variant v -> write_variant buffer i v

  and write_variant buffer i v =
    let i = write_types_signature buffer i [typ v] in
      write_value buffer i v

  and write buffer i = function
    | [] -> i
    | v :: vs ->
        let i = write_value buffer i v in
          write buffer i vs
end

module LEWriter = MakeWriter(Wire.LEWriter)
module BEWriter = MakeWriter(Wire.BEWriter)
module LEReader = MakeReader(Wire.LEReader)
module BEReader = MakeReader(Wire.BEReader)
