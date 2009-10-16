(*
 * oBus_value.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

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

(* +-----------------------------------------------------------------+
   | D-Bus type definitions                                          |
   +-----------------------------------------------------------------+ *)

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
 with constructor

type tsingle =
  | Tbasic of tbasic
  | Tstructure of tsingle list
  | Tarray of tsingle
  | Tdict of tbasic * tsingle
  | Tvariant
 with constructor

type tsequence = tsingle list

type signature = tsequence

(* +-----------------------------------------------------------------+
   | D-Bus types pretty-printing                                     |
   +-----------------------------------------------------------------+ *)

let string_of_tbasic = function
  | Tbyte -> "Tbyte"
  | Tboolean -> "Tboolean"
  | Tint16 -> "Tint16"
  | Tint32 -> "Tint32"
  | Tint64 -> "Tint64"
  | Tuint16 -> "Tuint16"
  | Tuint32 -> "Tuint32"
  | Tuint64 -> "Tuint64"
  | Tdouble -> "Tdouble"
  | Tstring -> "Tstring"
  | Tsignature -> "Tsignature"
  | Tobject_path -> "Tobject_path"

let print_tbasic pp t = pp_print_string pp (string_of_tbasic t)

let rec print_tsingle pp = function
  | Tbasic t -> fprintf pp "@[<2>Tbasic@ %a@]" print_tbasic t
  | Tarray t -> fprintf pp "@[<2>Tarray@,(%a)@]" print_tsingle t
  | Tdict(tk, tv) -> fprintf pp "@[<2>Tdict@,(@[<hv>%a,@ %a@])@]" print_tbasic tk print_tsingle tv
  | Tstructure tl -> fprintf pp "@[<2>Tstruct@ %a@]" print_tsequence tl
  | Tvariant -> fprintf pp "Tvariant"

and print_tsequence pp = print_list print_tsingle pp

(* +-----------------------------------------------------------------+
   | Signature validation                                            |
   +-----------------------------------------------------------------+ *)

type validate_part_result =
  | OK of int
  | Error of string

let length_validate_signature l =
  let rec aux_single length depth_struct depth_array depth_dict_entry = function
    | Tbasic _ | Tvariant ->
        length + 1
    | Tarray t ->
        if depth_array > OBus_constant.max_type_recursion_depth then
          failwith "too many nested arrays"
        else
          aux_single (length + 1) depth_struct (depth_array + 1) depth_dict_entry t
    | Tdict(tk, tv) ->
        if depth_array > OBus_constant.max_type_recursion_depth then
          failwith "too many nested arrays"
        else if depth_dict_entry > OBus_constant.max_type_recursion_depth then
          failwith "too many nested dict-entries"
        else
          aux_single (length + 4) depth_struct (depth_array + 1) (depth_dict_entry + 1) tv
    | Tstructure [] ->
        failwith "empty struct"
    | Tstructure tl ->
        if depth_struct > OBus_constant.max_type_recursion_depth then
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

let validate_signature l =
  try
    let _ = length_validate_signature l in
    None
  with Failure msg ->
    Some msg

(* +-----------------------------------------------------------------+
   | Signature reading                                               |
   +-----------------------------------------------------------------+ *)

let signature_of_string str =
  let len = String.length str and i = ref 0 in
  let fail fmt = Printf.ksprintf failwith ("OBus_value.signature_of_string: " ^^ fmt) in
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
    | chr -> fail msg chr
  in

  let rec parse_single = function
    | 'a' -> begin
        match get_char () with
          | '{' ->
              let tk = parse_basic "invalid basic type code: %c" (get_char ()) in
              let tv = parse_single (get_char ()) in
              begin match get_char () with
                | '}' -> Tdict(tk, tv)
                | _ -> fail "'}' missing"
              end
          | ch ->
              Tarray(parse_single ch)
      end
    | '(' ->
        Tstructure (parse_struct (get_char ()))
    | ')' ->
        fail "')' without '('"
    | 'v' ->
        Tvariant
    | ch ->
        Tbasic(parse_basic "invalid type code: %c" ch);

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
        Printf.ksprintf failwith "OBus_value.signature_of_string: invalid signature: %s" msg
    | None ->
        s

(* +-----------------------------------------------------------------+
   | Signature writing                                               |
   +-----------------------------------------------------------------+ *)

let string_of_signature signature =
  let len =
    try
      length_validate_signature signature
    with Failure msg ->
      Printf.ksprintf failwith "OBus_value.string_of_signature: invalid signature: %s" msg
  in
  let str = String.create len and i = ref 0 in
  let put_char ch =
    let j = !i in
    String.unsafe_set str j ch;
    i := j + 1
  in
  let write_basic t =
    put_char (match t with
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
                | Tsignature -> 'g')
  in
  let rec write_single = function
    | Tbasic t ->
        write_basic t
    | Tarray t ->
        put_char 'a';
        write_single t
    | Tdict(tk, tv) ->
        put_char 'a';
        put_char '{';
        write_basic tk;
        write_single tv;
        put_char '}'
    | Tstructure tl ->
        put_char '(';
        List.iter write_single tl;
        put_char ')'
    | Tvariant ->
        put_char 'v'
  in
  List.iter write_single signature;
  str

(* +-----------------------------------------------------------------+
   | D-Bus value definitions                                         |
   +-----------------------------------------------------------------+ *)

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
 with constructor

type single =
  | Basic of basic
  | Array of tsingle * single list
  | Byte_array of string
  | Dict of tbasic * tsingle * (basic * single) list
  | Structure of single list
  | Variant of single
 with constructor

type sequence = single list

let sbyte x = Basic(Byte x)
let sboolean x = Basic(Boolean x)
let sint16 x = Basic(Int16 x)
let sint32 x = Basic(Int32 x)
let sint64 x = Basic(Int64 x)
let suint16 x = Basic(Uint16 x)
let suint32 x = Basic(Uint32 x)
let suint64 x = Basic(Uint64 x)
let sdouble x = Basic(Double x)
let sstring x = Basic(String x)
let ssignature x = Basic(Signature x)
let sobject_path x = Basic(Object_path x)

(* +-----------------------------------------------------------------+
   | Value typing                                                    |
   +-----------------------------------------------------------------+ *)

let type_of_basic = function
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

let rec type_of_single = function
  | Basic x -> Tbasic(type_of_basic x)
  | Array(t, x) -> Tarray t
  | Byte_array x -> Tarray(Tbasic Tbyte)
  | Dict(tk, tv, x) -> Tdict(tk, tv)
  | Structure x -> Tstructure(List.map type_of_single x)
  | Variant _ -> Tvariant

let type_of_sequence = List.map type_of_single

let array t l =
  if t = Tbasic Tbyte then begin
    let s = String.create (List.length l) and i = ref 0 in
    List.iter (function
                 | Basic(Byte x) ->
                     String.unsafe_set s !i x;
                     incr i
                 | _ ->
                     invalid_arg "OBus_value.array: unexpected type") l;
    Byte_array s
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

(* +-----------------------------------------------------------------+
   | Value pretty-printing                                           |
   +-----------------------------------------------------------------+ *)

let print_basic pp = function
  | Byte x -> fprintf pp  "%C" x
  | Boolean x -> fprintf pp "%B" x
  | Int16 x -> fprintf pp "%d" x
  | Int32 x -> fprintf pp "%ldl" x
  | Int64 x -> fprintf pp "%LdL" x
  | Uint16 x -> fprintf pp "%d" x
  | Uint32 x -> fprintf pp "%ldl" x
  | Uint64 x -> fprintf pp "%LdL" x
  | Double x -> fprintf pp "%f" x
  | String x -> fprintf pp "%S" x
  | Signature x -> print_tsequence pp x
  | Object_path x -> print_list (fun pp elt -> fprintf pp "%S" elt) pp x

let explode str =
  let rec aux acc = function
    | -1 -> acc
    | i -> aux (Basic(Byte(String.unsafe_get str i)) :: acc) (i - 1)
  in
  aux [] (String.length str - 1)

let rec print_single pp = function
  | Basic v -> print_basic pp v
  | Array(t, l) -> print_list print_single pp l
  | Byte_array s -> print_single pp (Array(Tbasic Tbyte, explode s))
  | Dict(tk, tv, l) -> print_list (fun pp (k, v) -> fprintf pp "(@[%a,@ %a@])" print_basic k print_single v) pp l
  | Structure l -> print_sequence pp l
  | Variant x -> fprintf pp "@[<2>Variant@,(@[<hv>%a,@ %a@])@]" print_tsingle (type_of_single x) print_single x

and print_sequence pp l = print_tuple print_single pp l

let string_of printer x =
  let buf = Buffer.create 42 in
  let pp = formatter_of_buffer buf in
  pp_set_margin pp max_int;
  printer pp x;
  pp_print_flush pp ();
  Buffer.contents buf

let string_of_tsingle = string_of print_tsingle
let string_of_tsequence = string_of print_tsequence
let string_of_basic = string_of print_basic
let string_of_single = string_of print_single
let string_of_sequence = string_of print_sequence
