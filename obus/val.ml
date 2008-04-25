(*
 * val.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

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
  | Tarray of typ
  | Tdict of typ * typ
  | Tstructure of typ list
  | Tvariant

let rec string_of_type = function
  | Tbyte -> "Byte"
  | Tboolean -> "Boolean"
  | Tint16 -> "Int16"
  | Tint32 -> "Int32"
  | Tint64 -> "Int64"
  | Tuint16 -> "Uint16"
  | Tuint32 -> "Uint32"
  | Tuint64 -> "Uint64"
  | Tdouble -> "Double"
  | Tstring -> "String"
  | Tsignature -> "Signature"
  | Tobject_path -> "Object_path"
  | Tarray(t) -> "Array(" ^ string_of_type t ^ ")"
  | Tdict(tk, tv) -> "Dict(" ^ string_of_type tk ^ ", " ^ string_of_type tv ^ ")"
  | Tstructure(t) -> "Structure([" ^ String.concat "; " (List.map string_of_type t) ^ "])"
  | Tvariant -> "Variant"

type value =
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
  | Signature of typ list
  | Object_path of string
  | Array of typ * value list
  | Dict of typ * typ * (value * value) list
  | Structure of value list
  | Variant of value

let rec string_of_value = function
  | Byte(c) -> "Byte('" ^ Char.escaped c ^ "')"
  | Boolean(b) -> "Boolean(" ^ (if b then "true" else "false") ^ ")"
  | Int16(i) -> "Int16(" ^ string_of_int i ^ ")"
  | Int32(i) -> "Int32(" ^ Int32.to_string i ^ ")"
  | Int64(i) -> "Int64(" ^ Int64.to_string i ^ ")"
  | Uint16(i) -> "Uint16(" ^ string_of_int i ^ ")"
  | Uint32(i) -> "Uint32(" ^ Int32.to_string i ^ ")"
  | Uint64(i) -> "Uint64(" ^ Int64.to_string i ^ ")"
  | Double(d) -> "Double(" ^ string_of_float d ^ ")"
  | String(s) -> "String(\"" ^ s ^ "\")"
  | Signature(s) -> "Signature([" ^ String.concat "; " (List.map string_of_type s) ^ "])"
  | Object_path(s) -> "Object_path(" ^ s ^ ")"
  | Array(t, vs) -> "Array(" ^ string_of_type t ^ ", [" ^ String.concat "; " (List.map string_of_value vs) ^ "])"
  | Dict(tk, tv, vs) -> "Dict(" ^ string_of_type tk ^ ", " ^ string_of_type tv ^
      ", " ^ "[" ^ String.concat "; "
        (List.map (fun (k, v) -> string_of_value k ^ ", " ^ string_of_value v) vs) ^ "])"
  | Structure(t) -> "Structure([" ^ String.concat "; " (List.map string_of_value t) ^ "])"
  | Variant(x) -> "Variant(" ^ string_of_value x ^ ")"

let rec type_of_value = function
  | Byte _ -> Tbyte
  | Boolean _ -> Tboolean
  | Int16 _ -> Tint16
  | Int32 _ -> Tint32
  | Int64 _ -> Tint64
  | Uint16 _ -> Tuint16
  | Uint32 _ -> Tuint32
  | Uint64 _ -> Tuint64
  | Double _ -> Tdouble
  | String _ -> Tstring
  | Signature _ -> Tsignature
  | Object_path _ -> Tobject_path
  | Array(t, _) -> Tarray(t)
  | Dict(tk, tv, _) -> Tdict(tk, tv)
  | Structure(l) -> Tstructure(List.map type_of_value l)
  | Variant _ -> Tvariant

(** {6 DBus types/values construction} *)

type ('a, 'is_basic) cstr = typ * ('a -> value) * (value -> 'a)
type yes
type no
type 'a seq_cstr = typ list * ('a -> value list) * (value list -> 'a)

let invalid_type () = raise (Failure "invalid type")

let byte = (Tbyte,
            (fun x -> Byte x),
            (function
               | Byte x -> x
               | _ -> invalid_type ()))
let boolean = (Tboolean,
               (fun x -> Boolean x),
               (function
                  | Boolean x -> x
                  | _ -> invalid_type ()))
let int16 = (Tint16,
             (fun x -> Int16 x),
             (function
                | Int16 x -> x
                | _ -> invalid_type ()))
let int32 = (Tint32,
             (fun x -> Int32 x),
             (function
                | Int32 x -> x
                | _ -> invalid_type ()))
let int64 = (Tint64,
             (fun x -> Int64 x),
             (function
                | Int64 x -> x
                | _ -> invalid_type ()))
let uint16 = (Tuint16,
              (fun x -> Uint16 x),
              (function
                 | Uint16 x -> x
                 | _ -> invalid_type ()))
let uint32 = (Tuint32,
              (fun x -> Uint32 x),
              (function
                 | Uint32 x -> x
                 | _ -> invalid_type ()))
let uint64 = (Tuint64,
              (fun x -> Uint64 x),
              (function
                 | Uint64 x -> x
                 | _ -> invalid_type ()))
let double = (Tdouble,
              (fun x -> Double x),
              (function
                 | Double x -> x
                 | _ -> invalid_type ()))
let string = (Tstring,
              (fun x -> String x),
              (function
                 | String x -> x
                 | _ -> invalid_type ()))
let signature = (Tsignature,
                 (fun x -> Signature x),
                 (function
                    | Signature x -> x
                    | _ -> invalid_type ()))
let object_path = (Tobject_path,
                   (fun x -> Object_path x),
                   (function
                      | Object_path x -> x
                      | _ -> invalid_type ()))
let array (t, f, g) = (Tarray t,
                       (fun x -> Array (t, List.map f x)),
                       (function
                          | Array(_, x) -> List.map g x
                          | _ -> invalid_type ()))
let dict (tk, fk, gk) (tv, fv, gv) = (Tdict(tk, tv),
                                      (fun x -> Dict(tk, tv, List.map (fun (k, v) -> (fk k, fv v)) x)),
                                      (function
                                         | Dict(_, _, x) -> List.map (fun (k, v) -> (gk k, gv v)) x
                                         | _ -> invalid_type ()))
let structure (t, f, g) = (Tstructure t,
                           (fun x -> Structure(f x)),
                           (function
                              | Structure x -> g x
                              | _ -> invalid_type ()))
let variant = (Tvariant,
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

let make_type (t, _, _) = t
let make_value (_, f, _) = f
let make_type_list = make_type
let make_value_list = make_value
let get (_, _, g) = g
let get_list = get

open Wire

module Writer(W : Writer) =
struct
  let rec aux i = function
    | Tbyte -> W.buffer.[i] <- 'y'; i + 1
    | Tboolean -> W.buffer.[i] <- 'b'; i + 1
    | Tint16 -> W.buffer.[i] <- 'n'; i + 1
    | Tuint16 -> W.buffer.[i] <- 'q'; i + 1
    | Tint32 -> W.buffer.[i] <- 'i'; i + 1
    | Tuint32 -> W.buffer.[i] <- 'u'; i + 1
    | Tint64 -> W.buffer.[i] <- 'x'; i + 1
    | Tuint64 -> W.buffer.[i] <- 't'; i + 1
    | Tdouble -> W.buffer.[i] <- 'd'; i + 1
    | Tstring -> W.buffer.[i] <- 's'; i + 1
    | Tobject_path -> W.buffer.[i] <- 'o'; i + 1
    | Tsignature -> W.buffer.[i] <- 'g'; i + 1
    | Tarray(t) ->
        W.buffer.[i] <- 'a';
        aux (i + 1) t
    | Tdict(tk, tv) ->
        W.buffer.[i] <- 'a';
        W.buffer.[i + 1] <- '{';
        let i = aux (i + 2) tk in
        let i = aux i tv in
          W.buffer.[i] <- '}';
          i + 1
    | Tstructure(t) ->
        W.buffer.[i] <- '(';
        let i = List.fold_left aux i t in
          W.buffer.[i] <- ')';
          i + 1
    | Tvariant ->
        W.buffer.[i] <- 'v';
        i + 1


  let typ i t =
    let j = aux (i + 1) t in
      W.buffer.[i] <- char_of_int (j - i);
      W.buffer.[j] <- '\x00';
      i + 1

  let typ_list i t =
    let j = List.fold_left aux (i + 1) t in
      W.buffer.[i] <- char_of_int (j - i);
      W.buffer.[j] <- '\x00';
      i + 1

  let rec write_array f i =
    let i = W.pad4 i in
    let j = i + 4 in
    let k = f j in
    let len = k - j in
      if len > Constant.max_array_size
      then raise (Write_error "array too big!")
      else begin
        W.int_int32 len i;
        k
      end

  let rec value i = function
    | Byte(v) -> W.buffer.[i] <- v; i + 1
    | Boolean(v) -> let i = W.pad4 i in W.bool_boolean i v; i + 4
    | Int16(v) -> let i = W.pad2 i in W.int_int16 i v; i + 2
    | Int32(v) -> let i = W.pad4 i in W.int32_int32 i v; i + 4
    | Int64(v) -> let i = W.pad8 i in W.int64_int64 i v; i + 8
    | Uint16(v) -> let i = W.pad2 i in W.int_uint16 i v; i + 2
    | Uint32(v) -> let i = W.pad4 i in W.int32_uint32 i v; i + 4
    | Uint64(v) -> let i = W.pad8 i in W.int64_uint64 i v; i + 8
    | Double(v) -> let i = W.pad8 i in W.float_double i v; i + 8
    | String(v) -> W.string_string (W.pad4 i) v
    | Signature(t) -> typ_list i t
    | Object_path(v) -> W.string_string (W.pad4 i) v
    | Array(_, v) -> write_array (fun i -> List.fold_left value i v) i
    | Dict(_, _, v) -> write_array (fun i -> List.fold_left (fun i (k, v) -> value (value (W.pad8 i) k) v) i v) i
    | Structure(v) -> List.fold_left value (W.pad8 i) v
    | Variant(v) -> let i = typ i (type_of_value v) in value i v

  let value_list = List.fold_left value
end

module Reader(R : Reader) =
struct
  let basic_of_char = function
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
    | c -> raise (Read_error (Printf.sprintf "unknown type code %c" c))

  let rec typ i =
    match R.buffer.[i] with
      | 'a' ->
          if R.buffer.[i + 1] = '{'
          then begin
            let tkey = basic_of_char R.buffer.[i + 2] in
            let i, tval = typ (i + 3) in
              if R.buffer.[i] <> '}'
              then raise (Read_error "'}' expected")
              else (i + 1, Tdict(tkey, tval))
          end else begin
            let i, t = typ i in
              (i, Tarray(t))
          end
      | '(' ->
          let i, t = read_until ')' i in
            (i, Tstructure(t))
      | 'v' -> (i + 1, Tvariant)
      | c -> (i + 1, basic_of_char c)

  and read_until cend i =
    if R.buffer.[i] = cend
    then (i + 1, [])
    else
      let i, hd = typ i in
      let i, tl = read_until cend i in
        (i, hd :: tl)

  let typ_list = read_until '\x00'

  let rec value i = function
    | Tbyte -> (i + 1, Byte(R.buffer.[i]))
    | Tboolean -> let i = R.pad4 i in (i + 4, Boolean(R.bool_boolean i))
    | Tint16 -> let i = R.pad2 i in (i + 2, Int16(R.int_int16 i))
    | Tint32 -> let i = R.pad4 i in (i + 4, Int32(R.int32_int32 i))
    | Tint64 -> let i = R.pad8 i in (i + 8, Int64(R.int64_int64 i))
    | Tuint16 -> let i = R.pad2 i in (i + 2, Uint16(R.int_uint16 i))
    | Tuint32 -> let i = R.pad4 i in (i + 4, Uint32(R.int32_uint32 i))
    | Tuint64 -> let i = R.pad8 i in (i + 8, Uint64(R.int64_uint64 i))
    | Tdouble -> let i = R.pad8 i in (i + 8, Double(R.float_double i))
    | Tstring -> let i, v = R.string_string (R.pad4 i) in (i, String(v))
    | Tsignature -> let len = int_of_char R.buffer.[i] in (i + 1 + len, Signature(snd (typ_list (i + 1))))
    | Tobject_path -> let i, v = R.string_string (R.pad4 i) in (i, Object_path(v))
    | Tarray(t) ->
        let i = R.pad4 i in
        let len = R.int_uint32 i in
          if len > Constant.max_array_size
          then raise (Read_error "array too big!")
          else
            let i, v = Wire.read_until begin fun i acc ->
              let i, v = value i t in
                (i, v :: acc)
            end [] (i + len) i in
              (i, Array(t, List.rev v))
    | Tdict(tk, tv) ->
        let i = R.pad4 i in
        let len = R.int_uint32 i in
          if len > Constant.max_array_size
          then raise (Read_error "array too big!")
          else
            let i = R.pad8 i in
            let i, v = Wire.read_until begin fun i acc ->
              let i, k = value (R.pad8 i) tk in
              let i, v = value i tv in
                (i, (k, v) :: acc)
            end [] (i + len) i in
              (i, Dict(tk, tv, List.rev v))
    | Tstructure(t) ->
        let i , v = List.fold_left begin fun (i, acc) t ->
          let i, v = value i t in
            (i, v :: acc)
        end (R.pad8 i, []) t in
          (i, Structure(List.rev v))
    | Tvariant ->
        let len = int_of_char R.buffer.[i] in
        let _, t = typ (i + 1) in
        let i, v = value (i + 2 + len) t in
          (i, Variant(v))

  let value_list i t =
    let i, v = List.fold_left begin fun (i,acc) t ->
      let i, v = value i t in
        (i, v :: acc)
    end (i, []) t in
      (i, List.rev v)
end

let write_value value byte_order buffer ptr =
  match byte_order with
    | Header.Little_endian ->
        let module W = Writer(LEWriter(struct let buffer = buffer end)) in
          W.value_list ptr value
    | Header.Big_endian ->
        let module W = Writer(BEWriter(struct let buffer = buffer end)) in
          W.value_list ptr value

let read_value header buffer ptr =
  let t = (match header.Header.fields.Header.signature with
             | Some s ->
                 let module R = Reader(LEReader(struct let buffer = "\x00" end)) in
                   snd (R.typ_list 0)
             | _ -> []) in
    snd (match header.Header.byte_order with
           | Header.Little_endian ->
               let module R = Reader(LEReader(struct let buffer = buffer end)) in
                 R.value_list ptr t
           | Header.Big_endian ->
               let module R = Reader(BEReader(struct let buffer = buffer end)) in
                 R.value_list ptr t)

