(*
 * caml.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

open Camlp4.PreCast

type caml_expr = Ast.expr
type caml_type = Ast.ctyp
type caml_patt = Ast.patt

type var = string

type yes
type no

type dbus_type =
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
  | Tarray
  | Tdict
  | Tstruct
  | Tvariant
  | Tcons

module Gen = Generate.Make
  (struct
     type var = string
     type left = dbus_type
     type right = Ast.ident
   end)
  (struct
     type t = caml_expr
   end)

let _loc = Loc.ghost

let f0 f = function
  | [] -> f
  | _ -> assert false

let f1 f = function
  | [x] -> f x
  | _ -> assert false

let f2 f = function
  | [x; y] -> f x y
  | _ -> assert false

let f3 f = function
  | [x; y; z] -> f x y z
  | _ -> assert false

let t0 t = Gen.EPLeft(t, [])
let t1 t x = Gen.EPLeft(t, [x])
let t2 t x y = Gen.EPLeft(t, [x; y])
let t3 t x y z = Gen.EPLeft(t, [x; y; z])

type ('is_basic, 'is_single) dbus_reader =
    { reader_from : Gen.either_pattern;
      reader_vars : Gen.right_pattern list;
      reader_expr : caml_expr list -> caml_expr }

type basic_reader = (yes, yes) dbus_reader
type single_reader = (no, yes) dbus_reader
type sequence_reader = (no, no) dbus_reader

let rec split_at n l = match n, l with
  | (0, l) -> ([], l)
  | (n, x :: l) -> let b, e = split_at (n-1) l in (x :: b, e)
  | _ -> raise (Invalid_argument "split_at")

let rec signature_of_term = function
  | Gen.EPLeft(t, args) -> begin match t, args with
      | Tbyte, [] -> "y"
      | Tboolean, [] -> "b"
      | Tint16, [] -> "n"
      | Tuint16, [] -> "q"
      | Tint32, [] -> "i"
      | Tuint32, [] -> "u"
      | Tint64, [] -> "x"
      | Tuint64, [] -> "t"
      | Tdouble, [] -> "d"
      | Tstring, [] -> "s"
      | Tobject_path, [] -> "o"
      | Tsignature, [] -> "g"
      | Tarray, [x] -> "a" ^ signature_of_term x
      | Tdict, [k; v] -> "a{" ^ signature_of_term k ^ signature_of_term v ^ "}"
      | Tstruct, [x] -> "(" ^ signature_of_term x ^ ")"
      | Tvariant, [] -> "v"
      | Tcons, [a; b] -> signature_of_term a ^ signature_of_term b
      | _ -> raise (Invalid_argument "signature_of_term")
    end
  | _ -> raise (Invalid_argument "signature_of_term")

let right_from_caml_type t =
  let rec aux = function
    | (<:ctyp< $id:t$ >>) -> ([], t)
    | (<:ctyp< $a$ $b$ >>) -> let args, id = aux b in
        (aux2 a :: args, id)
    | _ -> raise (Invalid_argument "caml_type")
  and aux2 = function
    | (<:ctyp< '$a$ >>) -> Gen.RPVar(a)
    | t -> let args, id = aux t in
        Gen.RPTerm(id, args)
  in
    aux2 t

let rint16 =
  { reader_from = t0 Tint16;
    reader_vars = [];
    reader_expr = f0 <:expr< read_int16 >> }
let rint32 =
  { reader_from = t0 Tint32;
    reader_vars = [];
    reader_expr = f0 <:expr< read_int32 >> }
let rint64 =
  { reader_from = t0 Tint64;
    reader_vars = [];
    reader_expr = f0 <:expr< read_int64 >> }
let ruint16 =
  { reader_from = t0 Tuint16;
    reader_vars = [];
    reader_expr = f0 <:expr< read_uint16 >> }
let ruint32 =
  { reader_from = t0 Tuint32;
    reader_vars = [];
    reader_expr = f0 <:expr< read_uint32 >> }
let ruint64 =
  { reader_from = t0 Tuint64;
    reader_vars = [];
    reader_expr = f0 <:expr< read_uint64 >> }
let rint32_as_int32 =
  { reader_from = t0 Tint32;
    reader_vars = [];
    reader_expr = f0 <:expr< read_int32_as_int32 >> }
let rint64_as_int64 =
  { reader_from = t0 Tint64;
    reader_vars = [];
    reader_expr = f0 <:expr< read_int64_as_int64 >> }
let ruint32_as_int32 =
  { reader_from = t0 Tuint32;
    reader_vars = [];
    reader_expr = f0 <:expr< read_uint32_as_int32 >> }
let ruint64_as_int64 =
  { reader_from = t0 Tuint64;
    reader_vars = [];
    reader_expr = f0 <:expr< read_uint64_as_int64 >> }
let rsignature =
  { reader_from = t0 Tsignature;
    reader_vars = [];
    reader_expr = f0 <:expr< read_signature >> }
let rsignature_as_string =
  { reader_from = t0 Tsignature;
    reader_vars = [];
    reader_expr = f0 <:expr< read_signature_as_string >> }
let rbyte =
  { reader_from = t0 Tbyte;
    reader_vars = [];
    reader_expr = f0 <:expr< read_byte >> }
let rboolean =
  { reader_from = t0 Tboolean;
    reader_vars = [];
    reader_expr = f0 <:expr< read_boolean >> }
let rdouble =
  { reader_from = t0 Tdouble;
    reader_vars = [];
    reader_expr = f0 <:expr< read_double >> }
let rstring =
  { reader_from = t0 Tstring;
    reader_vars = [];
    reader_expr = f0 <:expr< read_string >> }
let robject_path =
  { reader_from = t0 Tobject_path;
    reader_vars = [];
    reader_expr = f0 <:expr< read_object_path >> }
let rarray elt_reader f x =
  { reader_from = t1 Tarray elt_reader.reader_from;
    reader_vars = elt_reader.reader_vars;
    reader_expr = (fun l -> <:expr< read_array $elt_reader.reader_expr l$ $f$ $x$ >>) }
let rdict key_reader val_reader f x =
  { reader_from = t2 Tdict key_reader.reader_from val_reader.reader_from;
    reader_vars = key_reader.reader_vars @ val_reader.reader_vars;
    reader_expr = (fun l ->
                     let key_args, val_args = split_at (List.length key_reader.reader_vars) l in
                       <:expr< read_dict
                         $key_reader.reader_expr key_args$
                         $val_reader.reader_expr val_args$
                         $f$ $x$ >>) }
let rstruct reader =
  { reader_from = t1 Tstruct reader.reader_from;
    reader_vars = reader.reader_vars;
    reader_expr = (fun l -> <:expr< read_struct $reader.reader_expr l$ >>) }
let rvariant =
  { reader_from = t0 Tvariant;
    reader_vars = [];
    reader_expr = f0 <:expr< read_variant >> }
let rfixed_variant chooser branches =
  { reader_from = t0 Tvariant;
    reader_vars = [];
    reader_expr =
      f0 (Ast.ExMat(_loc,
                    chooser,
                    List.fold_left begin fun expr (patt, reader) ->
                      Ast.McOr(_loc,
                               Ast.McArr(_loc,
                                         patt,
                                         Ast.ExNil _loc,
                                         <:expr< read_fixed_variant
                                           $str:signature_of_term reader.reader_from$
                                           $reader.reader_expr []$ >>),
                               expr)
                    end (Ast.McNil _loc) branches)) }

(*                    List.fold_right begin fun

<:expr< match <:expr< chooser >> with
          [ $List.fold_left begin fun (patt, reader) ->
              Ast.RbSem(_loc, $patt$,
                        <:expr< read_fixed_variant
                          $signature_of_term reader.reader_from$
                          $reader.reader_expr []$ >>)

List.map begin fun (patt, reader) ->
                 <:patt< $patt$ -> read_fixed_variant
                                 $signature_of_term reader.reader_from$
                                 $reader.reader_expr []$ >>
               end branches$ |] ] >> }

let print_bitfield_from_char name items =
  let bit i = string_of_int (1 lsl i) in
  let aux_item expr (item,i) =
    let item = camlize_field item in
    let assign = <:expr< bit_value land $int:bit i$ <> 0 >> in
      Ast.RbSem (_loc,
                 Ast.RbEq (_loc,
                           Ast.IdLid (_loc, item),
                           assign
                          ),
                 expr
                )
  in
  let make_bitfield = Ast.ExRec(_loc, List.fold_left aux_item (Ast.RbNil _loc) items, Ast.ExNil _loc) in
  let fun_name = from_char_name name in
    <:str_item< let $lid:fun_name$ char =
      let bit_value = int_of_char char in
        $make_bitfield$
        >>*)

(*let rnil =
  { reader_from = t0 Tnil;
    reader_vars = [];
    reader_expr = f0 *)
let rcons a v b = (*match b.reader_from with
  | EPLeft(Tcons, _) ->*)
      { reader_from = t2 Tcons a.reader_from b.reader_from;
        reader_vars = a.reader_vars @ b.reader_vars;
        reader_expr = begin fun l ->
          let la, lb = split_at (List.length a.reader_vars) l in
            <:expr< fun i ->
              let i, $v$ = $a.reader_expr la$ i in
                $b.reader_expr lb$ i >>
        end }
(*  | _ ->
      { reader_from = t2 Tcons a.reader_from (t2 Tcons b.reader_from (Gen.EPVar "__tail"));
        reader_vars = Gen.EPVar "__tail" :: (a.reader_vars @ b.reader_vars);
        reader_expr = begin function
          | tail :: l ->
              let la, lb = split_at (List.length a.reader_vars) l in
                (<:expr< fun i ->
                   let i, $v$ = $a la$ i in
                   let i, vh = $b lb$ i in
                   let i, vt = $tail$ in
                     (vh, vt) >>)
          | _ -> assert false }*)
let rbind r v e =
  { reader_from = r.reader_from;
    reader_vars = r.reader_vars;
    reader_expr = begin fun l ->
      <:expr< fun i ->
        let i, $v$ = $r.reader_expr l$ i in
          i, $e$ >>
    end }
let rcaml t =
  { reader_from = Gen.EPRight(right_from_caml_type t);
    reader_vars = [right_from_caml_type t];
    reader_expr = f1 (fun x -> x) }
let rv x =
  { reader_from = Gen.EPVar(x);
    reader_vars = [Gen.RPVar(x)];
    reader_expr = f1 (fun x -> x) }

type reader = Gen.generator

let reader reader caml_type =
  Gen.make_generator
    reader.reader_from
    (right_from_caml_type caml_type)
    reader.reader_vars
    reader.reader_expr

let (-->) a b = reader a b

let default_readers = [
  rbyte --> <:ctyp< char >>;
  rboolean --> <:ctyp< bool >>;
  rint16 --> <:ctyp< int >>;
  rint32 --> <:ctyp< int >>;
  rint64 --> <:ctyp< int >>;
  rint32_as_int32 --> <:ctyp< int32 >>;
  rint64_as_int64 --> <:ctyp< int64 >>;
  ruint16 --> <:ctyp< int >>;
  ruint32 --> <:ctyp< int >>;
  ruint64 --> <:ctyp< int >>;
  ruint32_as_int32 --> <:ctyp< int32 >>;
  ruint64_as_int64 --> <:ctyp< int64 >>;
  rdouble --> <:ctyp< float >>;
  rstring --> <:ctyp< string >>;
  rsignature_as_string --> <:ctyp< string >>;
  rsignature --> <:ctyp< OBus.dbus_type >>;
  robject_path --> <:ctyp< into_string >>;
  rarray (rv"a") <:expr< (::) >> <:expr< [] >> --> <:ctyp< 'a list >>;
  rdict (rv"k") (rv"v") <:expr< fun k v l -> (k, v) :: l >> <:expr< [] >> --> <:ctyp< ('k, 'v) list >>;
  rvariant --> <:ctyp< OBus.dbus_value >>;
  rbind rbyte <:patt< x >> <:expr< int_of_char x >> --> <:ctyp< char >>
]

let rec left_from_dbus_type = function
  | DBus.Tbyte -> Gen.Term(Tbyte, [])
  | DBus.Tboolean -> Gen.Term(Tboolean, [])
  | DBus.Tint16 -> Gen.Term(Tint16, [])
  | DBus.Tint32 -> Gen.Term(Tint32, [])
  | DBus.Tint64 -> Gen.Term(Tint64, [])
  | DBus.Tuint16 -> Gen.Term(Tuint16, [])
  | DBus.Tuint32 -> Gen.Term(Tuint32, [])
  | DBus.Tuint64 -> Gen.Term(Tuint64, [])
  | DBus.Tdouble -> Gen.Term(Tdouble, [])
  | DBus.Tstring -> Gen.Term(Tstring, [])
  | DBus.Tsignature -> Gen.Term(Tsignature, [])
  | DBus.Tobject_path -> Gen.Term(Tobject_path, [])
  | DBus.Tarray(t) -> Gen.Term(Tarray, [left_from_dbus_type t])
  | DBus.Tdict(tk, tv) -> Gen.Term(Tdict, [left_from_dbus_type tk; left_from_dbus_type tv])
  | DBus.Tstruct(tl) -> Gen.Term(Tstruct, [])
(*                             match tl with
                               | [] -> []
                               | x::l -> [List.fold_left (fun l x -> Gen.Term(Tcons,  left_from_dbus_type t])*)
  | DBus.Tvariant -> Gen.Term(Tvariant, [])

let right_from_caml_type t =
  let rec aux = function
    | (<:ctyp< $id:t$ >>) -> ([], t)
    | (<:ctyp< $a$ $b$ >>) -> let args, id = aux b in
        (aux2 a :: args, id)
    | _ -> raise (Invalid_argument "caml_type")
  and aux2 t =
    let args, id = aux t in
      Gen.Term(id, args)
  in
    aux2 t

let generate readers dbust camlt =
  Gen.generate readers (left_from_dbus_type dbust) (right_from_caml_type camlt)
