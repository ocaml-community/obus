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
  | Tnil

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

type reader =
    { reader_from : Gen.either_pattern;
      reader_vars : Gen.right_pattern list;
      reader_expr : caml_expr list -> caml_expr }

let rec split_at n l = match n, l with
  | (0, l) -> ([], l)
  | (n, x :: l) -> let b, e = split_at (n-1) l in (x :: b, e)
  | _ -> raise (Invalid_argument "split_at")

let rec split_with_motif m l =
  let rec aux m l = match m, l with
    | [], l -> ([], l)
    | _ :: m, e :: l -> let la, lb = aux m l in (e :: la, lb)
    | _ -> assert false
  in
    match m, l with
      | [], [] -> []
      | tm :: hm, l -> let la, lb = aux tm l in la :: split_with_motif hm lb
      | _ -> assert false

let split2 a b l = f2 (fun x y -> (x, y)) (split_with_motif [a; b] l)
let split3 a b c l = f3 (fun x y z -> (x, y, z)) (split_with_motif [a; b; c] l)


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

let right_pattern_from_caml_type t =
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
                     let key_args, val_args = split2 key_reader.reader_vars val_reader.reader_vars l in
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
let rseq readers result =
  List.fold_right begin fun (patt, reader) acc ->
    { reader_from = t2 Tcons reader.reader_from acc.reader_from;
      reader_vars = reader.reader_vars @ acc.reader_vars;
      reader_expr = fun l ->
        let la, lb = split2 reader.reader_vars acc.reader_vars l in
          <:expr<
            let i, $patt$ = $reader.reader_expr la$ i in
              $acc.reader_expr lb$ i >> }
  end readers { reader_from = t0 Tnil;
                reader_vars = [];
                reader_expr = f0 result }
let rbind r v e =
  { reader_from = r.reader_from;
    reader_vars = r.reader_vars;
    reader_expr = begin fun l ->
      <:expr< fun i ->
        let i, $v$ = $r.reader_expr l$ i in
          i, $e$ >>
    end }
let rcaml t =
  let rp = right_pattern_from_caml_type t in
    { reader_from = Gen.EPRight(rp);
      reader_vars = [rp];
      reader_expr = f1 (fun x -> x) }
let rv x =
  { reader_from = Gen.EPVar(x);
    reader_vars = [Gen.RPVar(x)];
    reader_expr = f1 (fun x -> x) }

type reading_rule = Gen.generator

let make_reading_rule reader caml_type =
  Gen.make_generator
    reader.reader_from
    (right_pattern_from_caml_type caml_type)
    reader.reader_vars
    reader.reader_expr

let (-->) a b = make_reading_rule a b

let default_reading_rules = [
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
  robject_path --> <:ctyp< string >>;
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
  | DBus.Tstruct(tl) ->
      Gen.Term(Tstruct,
               [List.fold_right (fun t acc ->
                                   Gen.Term(Tcons, [left_from_dbus_type t; acc]))
                  tl (Gen.Term(Tnil, []))])
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

let generate_reader rules dbust camlt =
  Gen.generate rules (left_from_dbus_type dbust) (right_from_caml_type camlt)

type writer =
    { writer_to : Gen.either_pattern;
      writer_vars : Gen.right_pattern list;
      writer_expr : caml_expr list -> caml_expr }

let wint16 =
  { writer_to = t0 Tint16;
    writer_vars = [];
    writer_expr = f0 <:expr< write_int16 >> }
let wint32 =
  { writer_to = t0 Tint32;
    writer_vars = [];
    writer_expr = f0 <:expr< write_int32 >> }
let wint64 =
  { writer_to = t0 Tint64;
    writer_vars = [];
    writer_expr = f0 <:expr< write_int64 >> }
let wuint16=
  { writer_to = t0 Tuint16;
    writer_vars = [];
    writer_expr = f0 <:expr< write_uint16 >> }
let wuint32=
  { writer_to = t0 Tuint32;
    writer_vars = [];
    writer_expr = f0 <:expr< write_uint32 >> }
let wuint64=
  { writer_to = t0 Tuint64;
    writer_vars = [];
    writer_expr = f0 <:expr< write_uint64 >> }
let wint32_from_int32 =
  { writer_to = t0 Tint32;
    writer_vars = [];
    writer_expr = f0 <:expr< write_int32_from_int32 >> }
let wint64_from_int64 =
  { writer_to = t0 Tint64;
    writer_vars = [];
    writer_expr = f0 <:expr< write_int64_from_int64 >> }
let wuint32_from_int32 =
  { writer_to = t0 Tuint32;
    writer_vars = [];
    writer_expr = f0 <:expr< write_uint32_from_int32 >> }
let wuint64_from_int64 =
  { writer_to = t0 Tuint64;
    writer_vars = [];
    writer_expr = f0 <:expr< write_uint64_from_int64 >> }
let wsignature =
  { writer_to = t0 Tsignature;
    writer_vars = [];
    writer_expr = f0 <:expr< write_signature >> }
let wsignature_from_string =
  { writer_to = t0 Tsignature;
    writer_vars = [];
    writer_expr = f0 <:expr< write_signature_from_string >> }
let wbyte =
  { writer_to = t0 Tbyte;
    writer_vars = [];
    writer_expr = f0 <:expr< write_byte >> }
let wboolean =
  { writer_to = t0 Tboolean;
    writer_vars = [];
    writer_expr = f0 <:expr< write_boolean >> }
let wdouble =
  { writer_to = t0 Tdouble;
    writer_vars = [];
    writer_expr = f0 <:expr< write_double >> }
let wstring =
  { writer_to = t0 Tstring;
    writer_vars = [];
    writer_expr = f0 <:expr< write_string >> }
let wobject_path =
  { writer_to = t0 Tobject_path;
    writer_vars = [];
    writer_expr = f0 <:expr< write_object_path >> }
let warray elt_writer fold =
  { writer_to = t1 Tarray elt_writer.writer_to;
    writer_vars = elt_writer.writer_vars;
    writer_expr = fun l ->
      <:expr< write_array
        $elt_writer.writer_expr l$
        $fold$ >> }
let wdict key_writer val_writer fold =
  { writer_to = t2 Tdict key_writer.writer_to val_writer.writer_to;
    writer_vars = key_writer.writer_vars @ val_writer.writer_vars;
    writer_expr = fun l ->
      let lk, lv = split2 key_writer.writer_vars val_writer.writer_vars l in
        <:expr< write_dict
          $key_writer.writer_expr lk$
          $val_writer.writer_expr lv$
          $fold$ >> }
let wstruct writer =
  { writer_to = t1 Tstruct writer.writer_to;
    writer_vars = writer.writer_vars;
    writer_expr = fun l ->
      <:expr< write_struct $writer.writer_expr l$ >> }
let wvariant =
  { writer_to = t0 Tvariant;
    writer_vars = [];
    writer_expr = f0 <:expr< write_variant >> }
let wfixed_variant branches common_writer =
  { writer_to = t0 Tvariant;
    writer_vars = common_writer.writer_vars;
    writer_expr = fun l ->
      <:expr< fun x i ->
        $Ast.ExMat(_loc,
                   <:expr< x >>,
                   List.fold_left begin fun acc (patt, common_expr, expr, writer) ->
                     Ast.McOr(_loc,
                              Ast.McArr(_loc,
                                        patt,
                                        Ast.ExNil _loc,
                                        <:expr<
                                          let i = $common_writer.writer_expr l$ $common_expr$ i in
                                            $writer.writer_expr []$ $expr$ i
                                            >>),
                              acc)
                   end (Ast.McNil _loc) branches)$ >> }
let wcons a b =
  { writer_to = t2 Tcons a.writer_to b.writer_to;
    writer_vars = a.writer_vars @ b.writer_vars;
    writer_expr = fun l ->
      let la, lb = split2 a.writer_vars b.writer_vars l in
        <:expr< fun (x,y) i ->
          let i = $a.writer_expr la$ x i in
            $b.writer_expr lb$ x i >> }
let wcaml t =
  let rp = right_pattern_from_caml_type t in
    { writer_to = Gen.EPRight(rp);
      writer_vars = [rp];
      writer_expr = f1 (fun x -> x) }
let wv v =
  { writer_to = Gen.EPVar v;
    writer_vars = [Gen.RPVar v];
    writer_expr = f1 (fun x -> x) }
let wconv expr writer =
  { writer_to = writer.writer_to;
    writer_vars = writer.writer_vars;
    writer_expr = fun l ->
      <:expr< fun x -> $writer.writer_expr l$ ($expr$ x) >> }

type writing_rule = Gen.generator

let make_writing_rule writer caml_type =
  Gen.make_generator
    writer.writer_to
    (right_pattern_from_caml_type caml_type)
    writer.writer_vars
    writer.writer_expr

let generate_writer rules camlt dbust =
  Gen.generate rules (left_from_dbus_type dbust) (right_from_caml_type camlt)

let (<--) a b = make_writing_rule a b

let default_writing_rules = [
  wbyte <-- <:ctyp< char >>;
  wboolean <-- <:ctyp< bool >>;
  wint16 <-- <:ctyp< int >>;
  wint32 <-- <:ctyp< int >>;
  wint64 <-- <:ctyp< int >>;
  wint32_from_int32 <-- <:ctyp< int32 >>;
  wint64_from_int64 <-- <:ctyp< int64 >>;
  wuint16 <-- <:ctyp< int >>;
  wuint32 <-- <:ctyp< int >>;
  wuint64 <-- <:ctyp< int >>;
  wuint32_from_int32 <-- <:ctyp< int32 >>;
  wuint64_from_int64 <-- <:ctyp< int64 >>;
  wdouble <-- <:ctyp< float >>;
  wstring <-- <:ctyp< string >>;
  wsignature_from_string <-- <:ctyp< string >>;
  wsignature <-- <:ctyp< OBus.dbus_type >>;
  wobject_path <-- <:ctyp< string >>;
  warray (wv"a") (<:expr< fun f l x -> List.fold_left f x l >>) <-- <:ctyp< 'a list >>;
  wdict (wv"k") (wv"v") (<:expr< fun f l x -> List.fold_left (fun acc (k, v) -> f k v acc) f l x >>) <-- <:ctyp< ('k, 'v) list >>;
  wvariant <-- <:ctyp< OBus.dbus_value >>;
  wconv <:expr< char_of_int >> (wcaml <:ctyp< char >>) <-- <:ctyp< int >>
]


let map_reading mod_name key_type =
  let mod_expr = <:ident< $uid:String.capitalize mod_name$ >> in
    rdict (rcaml key_type) (rv"a")
      (<:expr< $id:mod_expr$ . add >>) (<:expr< $id:mod_expr$ . empty >>)
    --> Ast.TyId(_loc, Ast.IdAcc(_loc, mod_expr, Ast.IdLid(_loc, "i")))

let map_writing mod_name key_type =
  let mod_expr = <:ident< $uid:String.capitalize mod_name$ >> in
    wdict (wcaml key_type) (wv"a")
      (<:expr< $id:mod_expr$ . fold >>) <-- Ast.TyId(_loc, Ast.IdAcc(_loc, mod_expr, Ast.IdLid(_loc, "i")))
