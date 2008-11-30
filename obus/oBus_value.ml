(*
 * oBus_value.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Format

module T =
struct
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

include T

type signature = tsequence

let string_of printer x =
  let buf = Buffer.create 42 in
  let pp = formatter_of_buffer buf in
  printer pp x;
  pp_print_flush pp ();
  Buffer.contents buf

let rec print_seq sep f pp = function
  | [] -> ()
  | [x] -> f pp x
  | x :: l ->
      f pp x;
      pp_print_string pp sep;
      print_seq sep f pp l

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
  | Tbasic t -> fprintf pp "Tbasic %a" print_tbasic t
  | Tarray t -> fprintf pp "Tarray(%a)" print_telement t
  | Tstruct tl -> fprintf pp "Tstruct %a" print_tsequence tl
  | Tvariant -> fprintf pp "Tvariant"

and print_telement pp = function
  | Tdict_entry(tk, tv) -> fprintf pp "Tdict_entry(%a, %a)" print_tbasic tk print_tsingle tv
  | Tsingle t -> fprintf pp "Tsingle(%a)" print_tsingle t

and print_tsequence pp tl = fprintf pp "[%a]" (print_seq "; " print_tsingle) tl


type ptr = { buffer : string; mutable offset : int }

module Ptr =
struct
  type 'a t = ptr -> 'a
  let bind m f ptr = f (m ptr) ptr
  let return x _ = x
end

module TR = Types_rw.Make_reader(T)
  (struct
     include Ptr
     let failwith msg _ = Pervasives.failwith msg
     let get_char ptr =
       let n = ptr.offset in
       if n = String.length ptr.buffer then
         Pervasives.failwith "premature end of signature"
       else begin
         ptr.offset <- n + 1;
         String.unsafe_get ptr.buffer n
       end
     let eof ptr = ptr.offset = String.length ptr.buffer
   end)

module TW = Types_rw.Make_writer(T)
  (struct
     include Ptr
     let put_char ch ptr =
       let n = ptr.offset in
       (* The size of the signature is computed before so there is
          always enough room *)
       String.unsafe_set ptr.buffer n ch;
       ptr.offset <- n + 1
   end)

let string_of_signature ts =
  let len, writer = TW.write_sequence ts in
  let str = String.create len in
  writer { buffer = str; offset = 0 };
  str

let signature_of_string str =
  try
    TR.read_sequence { buffer = str; offset = 0 }
  with
      Failure msg ->
        raise (Invalid_argument
                 (sprintf "signature_of_string: invalid signature(%S): %s" str msg))

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

type single =
  | Basic of basic
  | Array of telement * element list
  | Struct of single list
  | Variant of single

and element =
  | Dict_entry of basic * single
  | Single of single

type sequence = single list

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
  | Struct x -> Tstruct(List.map type_of_single x)
  | Variant _ -> Tvariant

let type_of_element = function
  | Dict_entry(k, v) -> Tdict_entry(type_of_basic k, type_of_single v)
  | Single x -> Tsingle(type_of_single x)

let type_of_sequence = List.map type_of_single

let vbyte x = Byte x
let vboolean x = Boolean x
let vint16 x = Int16 x
let vint32 x = Int32 x
let vint64 x = Int64 x
let vuint16 x = Uint16 x
let vuint32 x = Uint32 x
let vuint64 x = Uint64 x
let vdouble x = Double x
let vstring x = String x
let vsignature x = Signature x
let vobject_path x = Object_path x
let vbasic x = Basic x
let varray t l =
  List.iter (fun x ->
               if type_of_element x <> t
               then failwith "OBus_value.varray: unexpected type") l;
  Array(t, l)
let vstruct l = Struct l
let vvariant v = Variant v

let vdict_entry k v = Dict_entry(k, v)
let vsingle x = Single x

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
  | Object_path x -> fprintf pp "[%a]" (print_seq "; " (fun pp elt -> fprintf pp "%S" elt)) x

let rec print_single pp = function
  | Basic v -> print_basic pp v
  | Array(t, l) -> fprintf pp "[%a]" (print_seq "; "  print_element) l
  | Struct l -> print_sequence pp l
  | Variant x -> fprintf pp "Variant(%a, %a)" print_tsingle (type_of_single x) print_single x

and print_element pp = function
  | Dict_entry(k, v) -> fprintf pp "(%a, %a)" print_basic k print_single v
  | Single x -> print_single pp x

and print_sequence pp l = fprintf pp "(%a)" (print_seq ", " print_single) l

let string_of_tsingle = string_of print_tsingle
let string_of_telement = string_of print_telement
let string_of_tsequence = string_of print_tsequence
let string_of_basic = string_of print_basic
let string_of_single = string_of print_single
let string_of_element = string_of print_element
let string_of_sequence = string_of print_sequence
