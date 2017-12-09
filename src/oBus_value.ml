(*
 * oBus_value.mlp
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(value)"

open Format

let rec print_seq left right sep f pp l =
  pp_print_string pp left;
  begin match l with
    | [] -> ()
    | x :: l ->
        pp_open_box pp 0;
        f pp x;
        List.iter (fprintf pp "%s@ %a" sep f) l;
        pp_close_box pp ()
  end;
  pp_print_string pp right

let print_list f = print_seq "[" "]" ";" f
let print_tuple f = print_seq "(" ")" "," f

let string_of printer x =
  let buf = Buffer.create 42 in
  let pp = formatter_of_buffer buf in
  pp_set_margin pp max_int;
  printer pp x;
  pp_print_flush pp ();
  Buffer.contents buf

module T =
struct

  (* +---------------------------------------------------------------+
     | D-Bus type definitions |
     +---------------------------------------------------------------+ *)

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
    | Unix_fd

  type single =
    | Basic of basic
    | Structure of single list
    | Array of single
    | Dict of basic * single
    | Variant

  type sequence = single list

  let byte = Byte
  let boolean = Boolean
  let int16 = Int16
  let int32 = Int32
  let int64 = Int64
  let uint16 = Uint16
  let uint32 = Uint32
  let uint64 = Uint64
  let double = Double
  let string = String
  let signature = Signature
  let object_path = Object_path
  let unix_fd = Unix_fd

  let basic_byte = Basic Byte
  let basic_boolean = Basic Boolean
  let basic_int16 = Basic Int16
  let basic_int32 = Basic Int32
  let basic_int64 = Basic Int64
  let basic_uint16 = Basic Uint16
  let basic_uint32 = Basic Uint32
  let basic_uint64 = Basic Uint64
  let basic_double = Basic Double
  let basic_string = Basic String
  let basic_signature = Basic Signature
  let basic_object_path = Basic Object_path
  let basic_unix_fd = Basic Unix_fd

  let basic t = Basic t
  let structure t = Structure t
  let array t = Array t
  let dict tk tv = Dict(tk, tv)
  let variant = Variant

  (* +---------------------------------------------------------------+
     | D-Bus types pretty-printing |
     +---------------------------------------------------------------+ *)

  let string_of_basic = function
    | Byte -> "T.Byte"
    | Boolean -> "T.Boolean"
    | Int16 -> "T.Int16"
    | Int32 -> "T.Int32"
    | Int64 -> "T.Int64"
    | Uint16 -> "T.Uint16"
    | Uint32 -> "T.Uint32"
    | Uint64 -> "T.Uint64"
    | Double -> "T.Double"
    | String -> "T.String"
    | Signature -> "T.Signature"
    | Object_path -> "T.Object_path"
    | Unix_fd -> "T.Unix_fd"

  let print_basic pp t = pp_print_string pp (string_of_basic t)

  let rec print_single pp = function
    | Basic t -> fprintf pp "@[<2>T.Basic@ %a@]" print_basic t
    | Array t -> fprintf pp "@[<2>T.Array@,(%a)@]" print_single t
    | Dict(tk, tv) -> fprintf pp "@[<2>T.Dict@,(@[<hv>%a,@ %a@])@]" print_basic tk print_single tv
    | Structure tl -> fprintf pp "@[<2>T.Structure@ %a@]" print_sequence tl
    | Variant -> fprintf pp "T.Variant"

  and print_sequence pp = print_list print_single pp

  let string_of_single = string_of print_single
  let string_of_sequence = string_of print_sequence
end

type signature = T.sequence

(* +-----------------------------------------------------------------+
   | Signature validation |
   +-----------------------------------------------------------------+ *)

exception Invalid_signature of string * string

let () =
  Printexc.register_printer
    (function
       | Invalid_signature(str, msg) ->
           Some(Printf.sprintf "invalid signature %S: %s" str msg)
       | _ ->
           None)

let invalid_signature str msg = raise (Invalid_signature(str, msg))

let length_validate_signature l =
  let rec aux_single length depth_struct depth_array depth_dict_entry = function
    | T.Basic _ | T.Variant ->
        length + 1
    | T.Array t ->
        if depth_array > OBus_protocol.max_type_recursion_depth then
          failwith "too many nested arrays"
        else
          aux_single (length + 1) depth_struct (depth_array + 1) depth_dict_entry t
    | T.Dict(tk, tv) ->
        if depth_array > OBus_protocol.max_type_recursion_depth then
          failwith "too many nested arrays"
        else if depth_dict_entry > OBus_protocol.max_type_recursion_depth then
          failwith "too many nested dict-entries"
        else
          aux_single (length + 4) depth_struct (depth_array + 1) (depth_dict_entry + 1) tv
    | T.Structure [] ->
        failwith "empty struct"
    | T.Structure tl ->
        if depth_struct > OBus_protocol.max_type_recursion_depth then
          failwith "too many nested structs"
        else
          aux_sequence (length + 2) (depth_struct + 1) depth_array depth_dict_entry tl

  and aux_sequence length depth_struct depth_array depth_dict_entry = function
    | [] ->
        if length > 255 then
          failwith "signature too long"
        else
          length
    | t :: tl ->
        aux_sequence (aux_single length depth_struct depth_array depth_dict_entry t)
          depth_struct depth_array depth_dict_entry tl
  in
  aux_sequence 0 0 0 0 l

let signature_length l =
  let rec aux_single length = function
    | T.Basic _ | T.Variant ->
        length + 1
    | T.Array t ->
        aux_single (length + 1) t
    | T.Dict(tk, tv) ->
        aux_single (length + 4) tv
    | T.Structure tl ->
        aux_sequence (length + 2) tl
  and aux_sequence length = function
    | [] ->
        length
    | t :: tl ->
        aux_sequence (aux_single length t) tl
  in
  aux_sequence 0 l

let validate_signature l =
  try
    let _ = length_validate_signature l in
    None
  with Failure msg ->
    Some msg

(* +-----------------------------------------------------------------+
   | Signature reading |
   +-----------------------------------------------------------------+ *)

let signature_of_string str =
  let len = String.length str and i = ref 0 in
  let fail fmt = Printf.ksprintf (invalid_signature str) fmt in
  let get_char () =
    let j = !i in
    if j = len then
      fail "premature end of signature"
    else begin
      i := j + 1;
      String.unsafe_get str j
    end
  in
  let parse_basic msg = function
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
    | 'h' -> T.Unix_fd
    | chr -> fail msg chr
  in

  let rec parse_single = function
    | 'a' -> begin
        match get_char () with
          | '{' ->
              let tk = parse_basic "invalid basic type code: %c" (get_char ()) in
              let tv = parse_single (get_char ()) in
              begin match get_char () with
                | '}' -> T.Dict(tk, tv)
                | _ -> fail "'}' missing"
              end
          | ch ->
              T.Array(parse_single ch)
      end
    | '(' ->
        T.Structure (parse_struct (get_char ()))
    | ')' ->
        fail "')' without '('"
    | 'v' ->
        T.Variant
    | ch ->
        T.Basic(parse_basic "invalid type code: %c" ch);

  and parse_struct = function
    | ')' ->
        []
    | ch ->
        let t = parse_single ch in
        let l = parse_struct (get_char ()) in
        t :: l
  in

  let rec read_sequence () =
    if !i = len then
      []
    else
      let t = parse_single (get_char ()) in
      let l = read_sequence () in
      t :: l
  in
  let s = read_sequence () in
  match validate_signature s with
    | Some msg ->
        invalid_signature str msg
    | None ->
        s
(* +-----------------------------------------------------------------+
   | Signature writing |
   +-----------------------------------------------------------------+ *)

let string_of_signature signature =
  let len = signature_length signature in
  let str = Bytes.create len and i = ref 0 in
  let put_char ch =
    let j = !i in
    Bytes.unsafe_set str j ch;
    i := j + 1
  in
  let write_basic t =
    put_char (match t with
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
                | T.Unix_fd -> 'h')
  in
  let rec write_single = function
    | T.Basic t ->
        write_basic t
    | T.Array t ->
        put_char 'a';
        write_single t
    | T.Dict(tk, tv) ->
        put_char 'a';
        put_char '{';
        write_basic tk;
        write_single tv;
        put_char '}'
    | T.Structure tl ->
        put_char '(';
        List.iter write_single tl;
        put_char ')'
    | T.Variant ->
        put_char 'v'
  in
  List.iter write_single signature;
  let str = Bytes.unsafe_to_string str in
  try
    let _ = length_validate_signature in
    str
  with Failure msg ->
    raise (Invalid_signature(str, msg))

module V =
struct

  (* +---------------------------------------------------------------+
     | D-Bus value definitions |
     +---------------------------------------------------------------+ *)

  type basic =
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
    | Signature of signature
    | Object_path of OBus_path.t
    | Unix_fd of Unix.file_descr

  type single =
    | Basic of basic
    | Array of T.single * single list
    | Byte_array of string
    | Dict of T.basic * T.single * (basic * single) list
    | Structure of single list
    | Variant of single

  type sequence = single list

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
  let unix_fd x = Unix_fd x

  let basic_byte x = Basic(Byte x)
  let basic_boolean x = Basic(Boolean x)
  let basic_int16 x = Basic(Int16 x)
  let basic_int32 x = Basic(Int32 x)
  let basic_int64 x = Basic(Int64 x)
  let basic_uint16 x = Basic(Uint16 x)
  let basic_uint32 x = Basic(Uint32 x)
  let basic_uint64 x = Basic(Uint64 x)
  let basic_double x = Basic(Double x)
  let basic_string x = Basic(String x)
  let basic_signature x = Basic(Signature x)
  let basic_object_path x = Basic(Object_path x)
  let basic_unix_fd x = Basic(Unix_fd x)

  let basic x = Basic x
  let byte_array x = Byte_array x
  let structure x = Structure x
  let variant x = Variant x

  (* +---------------------------------------------------------------+
     | Value typing |
     +---------------------------------------------------------------+ *)

  let type_of_basic = function
    | Byte _ -> T.Byte
    | Boolean _ -> T.Boolean
    | Int16 _ -> T.Int16
    | Int32 _ -> T.Int32
    | Int64 _ -> T.Int64
    | Uint16 _ -> T.Uint16
    | Uint32 _ -> T.Uint32
    | Uint64 _ -> T.Uint64
    | Double _ -> T.Double
    | String _ -> T.String
    | Signature _ -> T.Signature
    | Object_path _ -> T.Object_path
    | Unix_fd _ -> T.Unix_fd

  let rec type_of_single = function
    | Basic x -> T.Basic(type_of_basic x)
    | Array(t, x) -> T.Array t
    | Byte_array x -> T.Array(T.Basic T.Byte)
    | Dict(tk, tv, x) -> T.Dict(tk, tv)
    | Structure x -> T.Structure(List.map type_of_single x)
    | Variant _ -> T.Variant

  let type_of_sequence = List.map type_of_single

  let array t l =
    if t = T.Basic T.Byte then begin
      let s = Bytes.create (List.length l) and i = ref 0 in
      List.iter (function
                   | Basic(Byte x) ->
                       Bytes.unsafe_set s !i x;
                       incr i
                   | _ ->
                       invalid_arg "OBus_value.array: unexpected type") l;
      Byte_array (Bytes.unsafe_to_string s)
    end else begin
      List.iter (fun x ->
                   if type_of_single x <> t then
                     invalid_arg "OBus_value.array: unexpected type") l;
      Array(t, l)
    end

  let dict tk tv l =
    List.iter (fun (k, v) ->
                 if type_of_basic k <> tk || type_of_single v <> tv then
                   invalid_arg "OBus_value.dict: unexpected type") l;
    Dict(tk, tv, l)

  let unsafe_array t l =
    if t = T.Basic T.Byte then
      array t l
    else
      Array(t, l)

  let unsafe_dict tk tv l =
    Dict(tk, tv, l)

  (* +---------------------------------------------------------------+
     | Value pretty-printing |
     +---------------------------------------------------------------+ *)

  let print_basic pp = function
    | Byte x -> fprintf pp "%C" x
    | Boolean x -> fprintf pp "%B" x
    | Int16 x -> fprintf pp "%d" x
    | Int32 x -> fprintf pp "%ldl" x
    | Int64 x -> fprintf pp "%LdL" x
    | Uint16 x -> fprintf pp "%d" x
    | Uint32 x -> fprintf pp "%ldl" x
    | Uint64 x -> fprintf pp "%LdL" x
    | Double x -> fprintf pp "%f" x
    | String x -> fprintf pp "%S" x
    | Signature x -> T.print_sequence pp x
    | Object_path x -> print_list (fun pp elt -> fprintf pp "%S" elt) pp x
    | Unix_fd x -> pp_print_string pp "<fd>"

  let explode str =
    let rec aux acc = function
      | -1 -> acc
      | i -> aux (Basic(Byte(String.unsafe_get str i)) :: acc) (i - 1)
    in
    aux [] (String.length str - 1)

  let rec print_single pp = function
    | Basic v -> print_basic pp v
    | Array(t, l) -> print_list print_single pp l
    | Byte_array s -> print_single pp (Array(T.Basic T.Byte, explode s))
    | Dict(tk, tv, l) -> print_list (fun pp (k, v) -> fprintf pp "(@[%a,@ %a@])" print_basic k print_single v) pp l
    | Structure l -> print_sequence pp l
    | Variant x -> fprintf pp "@[<2>Variant@,(@[<hv>%a,@ %a@])@]" T.print_single (type_of_single x) print_single x

  and print_sequence pp l = print_tuple print_single pp l

  let string_of_basic = string_of print_basic
  let string_of_single = string_of print_single
  let string_of_sequence = string_of print_sequence

  (* +---------------------------------------------------------------+
     | FDs closing |
     +---------------------------------------------------------------+ *)

  module FD_set = Set.Make(struct type t = Unix.file_descr let compare = compare end)

  let basic_contains_fds = function
    | T.Unix_fd -> true
    | _ -> false

  let rec single_contains_fds = function
    | T.Basic t -> basic_contains_fds t
    | T.Array t -> single_contains_fds t
    | T.Dict(tk, tv) -> basic_contains_fds tk || single_contains_fds tv
    | T.Structure t -> sequence_contains_fds t
    | T.Variant -> true

  and sequence_contains_fds t = List.exists single_contains_fds t

  let basic_collect_fds acc = function
    | Unix_fd fd -> FD_set .add fd acc
    | _ -> acc

  let rec single_collect_fds acc = function
    | Basic v ->
        basic_collect_fds acc v
    | Array(t, l) ->
        if single_contains_fds t then
          List.fold_left single_collect_fds acc l
        else
          acc
    | Dict(tk, tv, l) ->
        if basic_contains_fds tk || single_contains_fds tv then
          List.fold_left (fun acc (k, v) -> basic_collect_fds (single_collect_fds acc v) k) acc l
        else
          acc
    | Structure l ->
        sequence_collect_fds acc l
    | Variant v ->
        single_collect_fds acc v
    | Byte_array _ ->
        acc

  and sequence_collect_fds acc l =
    List.fold_left single_collect_fds acc l

  let close_fds collect_fds value =
    Lwt_list.iter_p
      (fun fd ->
         try
           Lwt_unix.close (Lwt_unix.of_unix_file_descr ~set_flags:false fd)
         with Unix.Unix_error(err, _, _) ->
           Lwt_log.error_f ~section "failed to close file descriptor: %s" (Unix.error_message err))
      (FD_set.elements (collect_fds FD_set.empty value))

  let basic_close = close_fds basic_collect_fds
  let single_close = close_fds single_collect_fds
  let sequence_close = close_fds sequence_collect_fds

  (* +---------------------------------------------------------------+
     | FDs duplicating |
     +---------------------------------------------------------------+ *)

  module FD_map = Map.Make(struct type t = Unix.file_descr let compare = compare end)

  let basic_dup map = function
    | Unix_fd fd -> begin
        try
          Unix_fd(FD_map.find fd !map)
        with Not_found ->
          let fd' = Unix.dup fd in
          map := FD_map.add fd fd' !map;
          Unix_fd fd
      end
    | value ->
        value

  let rec single_dup map = function
    | Basic x ->
        basic (basic_dup map x)
    | Array(t, l) as v ->
        if single_contains_fds t then
          array t (List.map (single_dup map) l)
        else
          v
    | Dict(tk, tv, l) as v ->
        if basic_contains_fds tk || single_contains_fds tv then
          dict tk tv (List.map (fun (k, v) -> (basic_dup map k, single_dup map v)) l)
        else
          v
    | Structure l ->
        structure (sequence_dup map l)
    | Byte_array _ as v ->
        v
    | Variant x ->
        variant (single_dup map x)

  and sequence_dup map l =
    List.map (single_dup map) l

  let basic_dup value = basic_dup (ref FD_map.empty) value
  let single_dup value = single_dup (ref FD_map.empty) value
  let sequence_dup value = sequence_dup (ref FD_map.empty) value
end

module C =
struct

  (* +---------------------------------------------------------------+
     | Type combinators |
     +---------------------------------------------------------------+ *)

  exception Signature_mismatch

  type 'a basic = {
    basic_type : T.basic;
    basic_make : 'a -> V.basic;
    basic_cast : V.basic -> 'a;
  }

  type 'a single = {
    single_type : T.single;
    single_make : 'a -> V.single;
    single_cast : V.single -> 'a;
  }

  type 'a sequence = {
    sequence_type : T.sequence;
    sequence_make : 'a -> V.sequence;
    sequence_cast : V.sequence -> 'a;
  }

  let type_basic t = t.basic_type
  let type_single t = t.single_type
  let type_sequence t = t.sequence_type

  let make_basic t x = t.basic_make x
  let make_single t x = t.single_make x
  let make_sequence t x = t.sequence_make x

  let cast_basic t x = t.basic_cast x
  let cast_single t x = t.single_cast x
  let cast_sequence t x = t.sequence_cast x

  let dyn_basic t = {
    basic_type = t;
    basic_make =
      (fun x ->
         if V.type_of_basic x <> t then
           failwith "OBus_value.dyn_basic: types mismatach"
         else
           x);
    basic_cast =
      (fun x ->
         if V.type_of_basic x <> t then
           raise Signature_mismatch
         else
           x);
  }

  let dyn_single t = {
    single_type = t;
    single_make =
      (fun x ->
         if V.type_of_single x <> t then
           failwith "OBus_value.dyn_single: types mismatach"
         else
           x);
    single_cast =
      (fun x ->
         if V.type_of_single x <> t then
           raise Signature_mismatch
         else
           x);
  }

  let dyn_sequence t = {
    sequence_type = t;
    sequence_make =
      (fun x ->
         if V.type_of_sequence x <> t then
           failwith "OBus_value.dyn_sequence: types mismatach"
         else
           x);
    sequence_cast =
      (fun x ->
         if V.type_of_sequence x <> t then
           raise Signature_mismatch
         else
           x);
  }

  let byte = {
    basic_type = T.Byte;
    basic_make = V.byte;
    basic_cast = (function
                    | V.Byte x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_byte = {
    single_type = T.basic_byte;
    single_make = V.basic_byte;
    single_cast = (function
                     | V.Basic(V.Byte x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let boolean = {
    basic_type = T.Boolean;
    basic_make = V.boolean;
    basic_cast = (function
                    | V.Boolean x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_boolean = {
    single_type = T.basic_boolean;
    single_make = V.basic_boolean;
    single_cast = (function
                     | V.Basic(V.Boolean x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let int16 = {
    basic_type = T.Int16;
    basic_make = V.int16;
    basic_cast = (function
                    | V.Int16 x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_int16 = {
    single_type = T.basic_int16;
    single_make = V.basic_int16;
    single_cast = (function
                     | V.Basic(V.Int16 x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let int32 = {
    basic_type = T.Int32;
    basic_make = V.int32;
    basic_cast = (function
                    | V.Int32 x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_int32 = {
    single_type = T.basic_int32;
    single_make = V.basic_int32;
    single_cast = (function
                     | V.Basic(V.Int32 x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let int64 = {
    basic_type = T.Int64;
    basic_make = V.int64;
    basic_cast = (function
                    | V.Int64 x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_int64 = {
    single_type = T.basic_int64;
    single_make = V.basic_int64;
    single_cast = (function
                     | V.Basic(V.Int64 x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let uint16 = {
    basic_type = T.Uint16;
    basic_make = V.uint16;
    basic_cast = (function
                    | V.Uint16 x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_uint16 = {
    single_type = T.basic_uint16;
    single_make = V.basic_uint16;
    single_cast = (function
                     | V.Basic(V.Uint16 x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let uint32 = {
    basic_type = T.Uint32;
    basic_make = V.uint32;
    basic_cast = (function
                    | V.Uint32 x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_uint32 = {
    single_type = T.basic_uint32;
    single_make = V.basic_uint32;
    single_cast = (function
                     | V.Basic(V.Uint32 x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let uint64 = {
    basic_type = T.Uint64;
    basic_make = V.uint64;
    basic_cast = (function
                    | V.Uint64 x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_uint64 = {
    single_type = T.basic_uint64;
    single_make = V.basic_uint64;
    single_cast = (function
                     | V.Basic(V.Uint64 x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let double = {
    basic_type = T.Double;
    basic_make = V.double;
    basic_cast = (function
                    | V.Double x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_double = {
    single_type = T.basic_double;
    single_make = V.basic_double;
    single_cast = (function
                     | V.Basic(V.Double x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let string = {
    basic_type = T.String;
    basic_make = V.string;
    basic_cast = (function
                    | V.String x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_string = {
    single_type = T.basic_string;
    single_make = V.basic_string;
    single_cast = (function
                     | V.Basic(V.String x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let signature = {
    basic_type = T.Signature;
    basic_make = V.signature;
    basic_cast = (function
                    | V.Signature x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_signature = {
    single_type = T.basic_signature;
    single_make = V.basic_signature;
    single_cast = (function
                     | V.Basic(V.Signature x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let object_path = {
    basic_type = T.Object_path;
    basic_make = V.object_path;
    basic_cast = (function
                    | V.Object_path x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_object_path = {
    single_type = T.basic_object_path;
    single_make = V.basic_object_path;
    single_cast = (function
                     | V.Basic(V.Object_path x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let unix_fd = {
    basic_type = T.Unix_fd;
    basic_make = V.unix_fd;
    basic_cast = (function
                    | V.Unix_fd x -> x
                    | _ -> raise Signature_mismatch);
  }

  let basic_unix_fd = {
    single_type = T.basic_unix_fd;
    single_make = V.basic_unix_fd;
    single_cast = (function
                     | V.Basic(V.Unix_fd x) -> x
                     | _ -> raise Signature_mismatch);
  }

  let basic t = {
    single_type = T.Basic t.basic_type;
    single_make = (fun x -> V.Basic(t.basic_make x));
    single_cast = (function
                     | V.Basic x -> t.basic_cast x
                     | _ -> raise Signature_mismatch);
  }

  let structure t = {
    single_type = T.Structure t.sequence_type;
    single_make = (fun x -> V.Structure(t.sequence_make x));
    single_cast = (function
                     | V.Structure x -> t.sequence_cast x
                     | _ -> raise Signature_mismatch);
  }

  let byte_array = {
    single_type = T.Array T.basic_byte;
    single_make = V.byte_array;
    single_cast = (function
                     | V.Byte_array x -> x
                     | _ -> raise Signature_mismatch);
  }

  let array t = {
    single_type = T.Array t.single_type;
    single_make = (fun x -> V.Array(t.single_type, List.map t.single_make x));
    single_cast = (function
                     | V.Array(t', x) when t.single_type = t' ->
                         List.map t.single_cast x
                     | V.Byte_array s when t.single_type = T.basic_byte ->
                         let rec aux acc = function
                           | -1 -> acc
                           | i -> aux (t.single_cast (V.basic_byte (String.unsafe_get s i)) :: acc) (i - 1)
                         in
                         aux [] (String.length s - 1)
                     | _ ->
                         raise Signature_mismatch);
  }

  let dict tk tv = {
    single_type = T.Dict(tk.basic_type, tv.single_type);
    single_make = (fun x -> V.Dict(tk.basic_type, tv.single_type, List.map (fun (k, v) -> (tk.basic_make k, tv.single_make v)) x));
    single_cast = (function
                     | V.Dict(tk', tv', x) when tk.basic_type = tk' && tv.single_type = tv' ->
                         List.map (fun (k, v) -> (tk.basic_cast k, tv.single_cast v)) x
                     | _ ->
                         raise Signature_mismatch);
  }

  let variant = {
    single_type = T.Variant;
    single_make = (fun x -> V.Variant x);
    single_cast = (function
                     | V.Variant x -> x
                     | _ -> raise Signature_mismatch);
  }

  let seq_cons t tl = {
    sequence_type = t.single_type :: tl.sequence_type;
    sequence_make = (fun (x, l) -> t.single_make x :: tl.sequence_make l);
    sequence_cast = (function
                       | x :: l -> (t.single_cast x, tl.sequence_cast l)
                       | [] -> raise Signature_mismatch);
  }

  let seq0 = {
    sequence_type = [];
    sequence_make = (fun () -> []);
    sequence_cast = (function
                       | [] -> ()
                       | _ -> raise Signature_mismatch);
  }
  let seq1 t1 = {
    sequence_type = [t1.single_type];
    sequence_make = (fun x1 -> [t1.single_make x1]);
    sequence_cast = (function
                       | [x1] -> t1.single_cast x1
                       | _ -> raise Signature_mismatch);
  }
  let seq2 t1 t2 = {
    sequence_type = [t1.single_type; t2.single_type];
    sequence_make = (fun (x1, x2) -> [t1.single_make x1; t2.single_make x2]);
    sequence_cast = (function
                       | [x1; x2] -> (t1.single_cast x1, t2.single_cast x2)
                       | _ -> raise Signature_mismatch);
  }
  let seq3 t1 t2 t3 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type];
    sequence_make = (fun (x1, x2, x3) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3]);
    sequence_cast = (function
                       | [x1; x2; x3] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3)
                       | _ -> raise Signature_mismatch);
  }
  let seq4 t1 t2 t3 t4 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type];
    sequence_make = (fun (x1, x2, x3, x4) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4]);
    sequence_cast = (function
                       | [x1; x2; x3; x4] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4)
                       | _ -> raise Signature_mismatch);
  }
  let seq5 t1 t2 t3 t4 t5 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5)
                       | _ -> raise Signature_mismatch);
  }
  let seq6 t1 t2 t3 t4 t5 t6 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6)
                       | _ -> raise Signature_mismatch);
  }
  let seq7 t1 t2 t3 t4 t5 t6 t7 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7)
                       | _ -> raise Signature_mismatch);
  }
  let seq8 t1 t2 t3 t4 t5 t6 t7 t8 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8)
                       | _ -> raise Signature_mismatch);
  }
  let seq9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type; t9.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8; t9.single_make x9]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8; x9] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8, t9.single_cast x9)
                       | _ -> raise Signature_mismatch);
  }
  let seq10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type; t9.single_type; t10.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8; t9.single_make x9; t10.single_make x10]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8, t9.single_cast x9, t10.single_cast x10)
                       | _ -> raise Signature_mismatch);
  }
  let seq11 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type; t9.single_type; t10.single_type; t11.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8; t9.single_make x9; t10.single_make x10; t11.single_make x11]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8, t9.single_cast x9, t10.single_cast x10, t11.single_cast x11)
                       | _ -> raise Signature_mismatch);
  }
  let seq12 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type; t9.single_type; t10.single_type; t11.single_type; t12.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8; t9.single_make x9; t10.single_make x10; t11.single_make x11; t12.single_make x12]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8, t9.single_cast x9, t10.single_cast x10, t11.single_cast x11, t12.single_cast x12)
                       | _ -> raise Signature_mismatch);
  }
  let seq13 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type; t9.single_type; t10.single_type; t11.single_type; t12.single_type; t13.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8; t9.single_make x9; t10.single_make x10; t11.single_make x11; t12.single_make x12; t13.single_make x13]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12; x13] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8, t9.single_cast x9, t10.single_cast x10, t11.single_cast x11, t12.single_cast x12, t13.single_cast x13)
                       | _ -> raise Signature_mismatch);
  }
  let seq14 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type; t9.single_type; t10.single_type; t11.single_type; t12.single_type; t13.single_type; t14.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8; t9.single_make x9; t10.single_make x10; t11.single_make x11; t12.single_make x12; t13.single_make x13; t14.single_make x14]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12; x13; x14] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8, t9.single_cast x9, t10.single_cast x10, t11.single_cast x11, t12.single_cast x12, t13.single_cast x13, t14.single_cast x14)
                       | _ -> raise Signature_mismatch);
  }
  let seq15 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type; t9.single_type; t10.single_type; t11.single_type; t12.single_type; t13.single_type; t14.single_type; t15.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8; t9.single_make x9; t10.single_make x10; t11.single_make x11; t12.single_make x12; t13.single_make x13; t14.single_make x14; t15.single_make x15]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12; x13; x14; x15] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8, t9.single_cast x9, t10.single_cast x10, t11.single_cast x11, t12.single_cast x12, t13.single_cast x13, t14.single_cast x14, t15.single_cast x15)
                       | _ -> raise Signature_mismatch);
  }
  let seq16 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 = {
    sequence_type = [t1.single_type; t2.single_type; t3.single_type; t4.single_type; t5.single_type; t6.single_type; t7.single_type; t8.single_type; t9.single_type; t10.single_type; t11.single_type; t12.single_type; t13.single_type; t14.single_type; t15.single_type; t16.single_type];
    sequence_make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) -> [t1.single_make x1; t2.single_make x2; t3.single_make x3; t4.single_make x4; t5.single_make x5; t6.single_make x6; t7.single_make x7; t8.single_make x8; t9.single_make x9; t10.single_make x10; t11.single_make x11; t12.single_make x12; t13.single_make x13; t14.single_make x14; t15.single_make x15; t16.single_make x16]);
    sequence_cast = (function
                       | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12; x13; x14; x15; x16] -> (t1.single_cast x1, t2.single_cast x2, t3.single_cast x3, t4.single_cast x4, t5.single_cast x5, t6.single_cast x6, t7.single_cast x7, t8.single_cast x8, t9.single_cast x9, t10.single_cast x10, t11.single_cast x11, t12.single_cast x12, t13.single_cast x13, t14.single_cast x14, t15.single_cast x15, t16.single_cast x16)
                       | _ -> raise Signature_mismatch);
  }
end

(* +-----------------------------------------------------------------+
   | Arguments                                                       |
   +-----------------------------------------------------------------+ *)

open C

type 'a arguments = {
  arg_types : 'a C.sequence;
  arg_names : string option list;
}

let arguments ~arg_types ~arg_names =
  if List.length arg_types.C.sequence_type = List.length arg_names then
    {
      arg_types = arg_types;
      arg_names = arg_names;
    }
  else
    invalid_arg "OBus_value.arguments"

let arg_types t = t.arg_types
let arg_names t = t.arg_names

let arg_cons (name, t) args = {
  arg_types = seq_cons t args.arg_types;
  arg_names = name :: args.arg_names;
}

let arg0 = {
  arg_types = seq0;
  arg_names = [];
}
let arg1 (n1, t1) = {
  arg_types = seq1 t1;
  arg_names = [n1];
}
let arg2 (n1, t1) (n2, t2) = {
  arg_types = seq2 t1 t2;
  arg_names = [n1; n2];
}
let arg3 (n1, t1) (n2, t2) (n3, t3) = {
  arg_types = seq3 t1 t2 t3;
  arg_names = [n1; n2; n3];
}
let arg4 (n1, t1) (n2, t2) (n3, t3) (n4, t4) = {
  arg_types = seq4 t1 t2 t3 t4;
  arg_names = [n1; n2; n3; n4];
}
let arg5 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) = {
  arg_types = seq5 t1 t2 t3 t4 t5;
  arg_names = [n1; n2; n3; n4; n5];
}
let arg6 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) = {
  arg_types = seq6 t1 t2 t3 t4 t5 t6;
  arg_names = [n1; n2; n3; n4; n5; n6];
}
let arg7 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) = {
  arg_types = seq7 t1 t2 t3 t4 t5 t6 t7;
  arg_names = [n1; n2; n3; n4; n5; n6; n7];
}
let arg8 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) = {
  arg_types = seq8 t1 t2 t3 t4 t5 t6 t7 t8;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8];
}
let arg9 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) (n9, t9) = {
  arg_types = seq9 t1 t2 t3 t4 t5 t6 t7 t8 t9;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8; n9];
}
let arg10 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) (n9, t9) (n10, t10) = {
  arg_types = seq10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8; n9; n10];
}
let arg11 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) (n9, t9) (n10, t10) (n11, t11) = {
  arg_types = seq11 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8; n9; n10; n11];
}
let arg12 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) (n9, t9) (n10, t10) (n11, t11) (n12, t12) = {
  arg_types = seq12 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8; n9; n10; n11; n12];
}
let arg13 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) (n9, t9) (n10, t10) (n11, t11) (n12, t12) (n13, t13) = {
  arg_types = seq13 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8; n9; n10; n11; n12; n13];
}
let arg14 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) (n9, t9) (n10, t10) (n11, t11) (n12, t12) (n13, t13) (n14, t14) = {
  arg_types = seq14 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8; n9; n10; n11; n12; n13; n14];
}
let arg15 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) (n9, t9) (n10, t10) (n11, t11) (n12, t12) (n13, t13) (n14, t14) (n15, t15) = {
  arg_types = seq15 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8; n9; n10; n11; n12; n13; n14; n15];
}
let arg16 (n1, t1) (n2, t2) (n3, t3) (n4, t4) (n5, t5) (n6, t6) (n7, t7) (n8, t8) (n9, t9) (n10, t10) (n11, t11) (n12, t12) (n13, t13) (n14, t14) (n15, t15) (n16, t16) = {
  arg_types = seq16 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16;
  arg_names = [n1; n2; n3; n4; n5; n6; n7; n8; n9; n10; n11; n12; n13; n14; n15; n16];
}

