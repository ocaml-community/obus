(*
 * caml.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

open Camlp4.PreCast

type expr = Ast.expr

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
     type right = string
   end)
  (struct
     type t = expr
   end)

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

type var = [ `Var of string ]
type 'a typ = [ `RTerm of string * 'a list ]
type poly = Gen.rpattern
type mono = Gen.rterm

type module_str = Ast.str_item
type module_sig = mono Interface.t

let t0 id = `RTerm(id, [])
let t1 id a0 = `RTerm(id, [a0])
let t2 id a0 a1 = `RTerm(id, [a0; a1])
let t3 id a0 a1 a2 = `RTerm(id, [a0; a1; a2])

let make_type id args = `RTerm(id, args)

let v x = `Var(x)
let int = t0 "int"
let int32 = t0 "int32"
let int64 = t0 "int64"
let float = t0 "float"
let bool = t0 "bool"
let char = t0 "char"
let string = t0 "string"
let list x = t1 "list" x
let array x = t1 "array" x
let dbus_value = t0 "OBus.dbus_value"
let dbus_type = t0 "OBus.dbus_type"
let tuple (l : 'a list) : [> 'a typ ] = match l with
  | [] -> t0 "unit"
  | [t] -> t
  | _ :: _ :: _ -> List.fold_right (fun t acc -> t2 "*cons" t acc) l (t0 "*nil")

let _loc = Loc.ghost

let dbyte = `LTerm(Tbyte, [])
let dboolean = `LTerm(Tboolean, [])
let dint16 = `LTerm(Tint16, [])
let dint32 = `LTerm(Tint32, [])
let dint64 = `LTerm(Tint64, [])
let duint16 = `LTerm(Tuint16, [])
let duint32 = `LTerm(Tuint32, [])
let duint64 = `LTerm(Tuint64, [])
let ddouble = `LTerm(Tdouble, [])
let dstring = `LTerm(Tstring, [])
let dsignature = `LTerm(Tsignature, [])
let dobject_path = `LTerm(Tobject_path, [])
let darray x = `LTerm(Tarray, [x])
let ddict k v = `LTerm(Tdict, [k; v])
let dstruct l = `LTerm(Tstruct, [l])
let dvariant = `LTerm(Tvariant, [])
let dcons e l = `LTerm(Tcons, [e; l])
let dnil = `LTerm(Tnil, [])

type reader_generator = DBus.typ -> mono -> expr
type writer_generator = mono -> DBus.typ -> expr
type convertion_rule = reader_generator -> writer_generator -> Gen.generator * Gen.generator

let rec term_of_dbus_type = function
  | DBus.Tbyte -> dbyte
  | DBus.Tboolean -> dboolean
  | DBus.Tint16 -> dint16
  | DBus.Tuint16 -> duint16
  | DBus.Tint32 -> dint32
  | DBus.Tuint32 -> duint32
  | DBus.Tint64 -> dint64
  | DBus.Tuint64 -> duint64
  | DBus.Tdouble -> ddouble
  | DBus.Tstring -> dstring
  | DBus.Tobject_path -> dobject_path
  | DBus.Tsignature -> dsignature
  | DBus.Tarray(x) -> darray (term_of_dbus_type x)
  | DBus.Tdict(k, v) -> ddict (term_of_dbus_type k) (term_of_dbus_type v)
  | DBus.Tstruct(l) -> dstruct (List.fold_right (fun x acc -> dcons (term_of_dbus_type x) acc) l dnil)
  | DBus.Tvariant -> dvariant

let make_rule left right vars reader writer =
  (Gen.make_generator left right vars reader,
   Gen.make_generator left right vars writer)

let rule_any (ctyp : poly) (ctyp_from : poly) read write _ _ =
  let rtype = ctyp_from
  and rvars = [ctyp_from]
  and rreader = f1 (fun reader -> <:expr< fun i -> let i, v = $reader$ i in (i, $read$ v) >>)
  and rwriter = f1 (fun writer -> <:expr< fun v i -> $writer$ ($write$ v) i >>) in
    make_rule (rtype : poly :> Gen.pattern) ctyp rvars rreader rwriter

let rule_array (ctyp : poly) (etyp : poly) empty add fold _ _ =
  let rtype = darray (etyp : poly :> Gen.pattern)
  and rvars = [etyp]
  and rreader = f1 (fun ereader -> <:expr< read_array $ereader$ $empty$ $add$ >>)
  and rwriter = f1 (fun ewriter -> <:expr< write_array $ewriter$ $fold$ >>) in
    make_rule rtype ctyp rvars rreader rwriter

let rule_dict (ctyp : poly) (ktyp : poly) (vtyp : poly) empty add fold _ _ =
  let rtype = ddict (ktyp : poly :> Gen.pattern) (vtyp : poly :> Gen.pattern)
  and rvars = [ktyp; vtyp]
  and rreader = f2 (fun kreader vreader -> <:expr< read_dict $kreader$ $vreader$ $empty$ $add$ >>)
  and rwriter = f2 (fun kwriter vwriter -> <:expr< write_array $kwriter$ $vwriter$ $fold$ >>) in
    make_rule rtype ctyp rvars rreader rwriter

let rule_record (ctyp : poly) (fields : (string * poly) list) _ _ =
  let rtype = List.fold_right (fun (_, ctyp) acc -> dcons (ctyp : poly :> Gen.pattern) acc) fields dnil
  and rvars = List.map snd fields;
  and rreader = fun l ->
    let l_with_names =
      snd (List.fold_right begin fun reader (i, l) ->
             (i + 1, ("v" ^ string_of_int i, reader) :: l)
           end l (0, [])) in
      List.fold_right begin fun (name, reader) acc ->
        (<:expr< fun i ->
           let i, $uid:name$ = $reader$ i in
             $acc$ >>)
      end l_with_names
        (Ast.ExTup(_loc,
                   Ast.ExCom(_loc,
                             Ast.ExId(_loc, Ast.IdLid(_loc, "i")),
                             (Ast.ExRec(_loc,
                                        (List.fold_left2
                                           (fun acc (field_name, _) (var_name, _) ->
                                              Ast.RbSem(_loc,
                                                        Ast.RbEq(_loc,
                                                                 Ast.IdLid(_loc, field_name),
                                                                 Ast.ExId(_loc, Ast.IdLid(_loc, var_name))),
                                                        acc))
                                           (Ast.RbNil _loc) fields l_with_names),
                                        Ast.ExNil _loc)))))
  and rwriter = fun l ->
    <:expr< (fun v i ->
               $(List.fold_right2 begin fun (name, _) writer acc ->
                   <:expr<
                     let i = $writer$ (v . $lid:name$) i in
                       $acc$ >>
                 end fields l <:expr< i >>)$)
    >> in
    make_rule rtype ctyp rvars rreader rwriter

let rule_variant (ctyp : poly) (variants : (int * string * DBus.typ * mono) list) gen_reader gen_writer =
  let rtype : Gen.pattern = dcons (int : mono :> Gen.pattern) (dcons dvariant dnil)
  and rvars = [(int : mono :> poly)]
  and rreader = f1 begin fun int_reader ->
    let match_expr =
      Ast.ExMat(_loc,
                Ast.ExId(_loc, Ast.IdLid(_loc, "chooser")),
                List.fold_left begin fun expr (n, cstr, dbust, paramt) ->
                  Ast.McOr(_loc,
                           Ast.McArr(_loc,
                                     Ast.PaInt(_loc, string_of_int n),
                                     Ast.ExNil _loc,
                                     let reader = gen_reader dbust paramt in
                                       <:expr< let i, v =
                                         read_fixed_variant $str:DBus.string_of_type dbust$ $reader$ i in
                                         (i, $uid:cstr$(v)) >>),
                           expr)
                end (Ast.McNil _loc) variants) in
      <:expr< fun i ->
        let i, chooser = $int_reader$ i in
          $match_expr$ >>
  end
  and rwriter = f1 begin fun int_writer ->
    let match_expr =
      Ast.ExMat(_loc,
                Ast.ExId(_loc, Ast.IdLid(_loc, "v")),
                List.fold_left begin fun expr (n, cstr, dbust, paramt) ->
                  Ast.McOr(_loc,
                           Ast.McArr(_loc,
                                     Ast.PaApp(_loc,
                                               Ast.PaId(_loc, Ast.IdUid(_loc, cstr)),
                                               Ast.PaId(_loc, Ast.IdLid(_loc, "x"))),
                                     Ast.ExNil _loc,
                                     let writer = gen_writer paramt dbust in
                                       <:expr< let i = $int_writer$ $int:string_of_int n$ i in
                                         write_fixed_variant $str:DBus.string_of_type dbust$ $writer$ x i >>),
                           expr)
                end (Ast.McNil _loc) variants) in
      <:expr< fun i -> $match_expr$ >> end in
    make_rule rtype ctyp rvars rreader rwriter


let rule_basic (ctyp : mono) (dtyp : Gen.lterm) reader writer _ _ =
  let rtype = (dtyp : Gen.lterm :> Gen.pattern)
  and rvars = []
  and rreader = f0 <:expr< $lid:reader$ >>
  and rwriter = f0 <:expr< $lid:writer$ >> in
    make_rule rtype (ctyp : mono :> poly) rvars rreader rwriter

let rule_map key_type mod_name =
  rule_dict (t1 (mod_name ^ ".t") (v"x")) key_type (v"x")
    (<:expr< $uid:mod_name$ . empty >>)
    (<:expr< $uid:mod_name$ . add >>)
    (<:expr< $uid:mod_name$ . fold >>)

let default_rules =
  [ (fun _ _ ->
       make_rule (dstruct (v"x")) (v"x") [v"x"]
         (f1 (fun r -> <:expr< read_struct $r$ >>))
         (f1 (fun w -> <:expr< write_struct $w$ >>)));
    rule_basic int dint16 "read_int16" "write_int16";
    rule_basic int duint16 "read_uint16" "write_uint16";
    rule_basic int dint32 "read_int32" "write_int32";
    rule_basic int duint32 "read_uint32" "write_uint32";
    rule_basic int dint64 "read_int32" "write_int64";
    rule_basic int duint64 "read_uint32" "write_uint64";
    rule_basic int32 dint32 "read_int32_as_int32" "write_int32_from_int32";
    rule_basic int32 duint32 "read_uint32_as_int32" "write_uint32_from_int32";
    rule_basic int64 dint64 "read_int64_as_int64" "write_int64_from_int64";
    rule_basic int64 duint64 "read_uint64_as_int64" "write_uint64_from_int64";
    rule_basic char dbyte "read_byte" "write_byte";
    rule_any (int : mono :> poly) (char : mono :> poly) (<:expr< int_of_char >>) (<:expr< char_of_int >>);
    rule_basic bool dboolean "read_boolean" "write_boolean";
    rule_basic float ddouble "read_double" "write_double";
    rule_basic string dstring "read_string" "write_string";
    rule_basic dbus_type dsignature "read_signature" "write_signature";
    rule_basic string dsignature "read_signature_as_string" "write_signature_from_string";
    rule_basic string dobject_path "read_object_path" "write_object_path";
    rule_array (list (v"x")) (v"x") (<:expr< [] >>) (<:expr< ( :: ) >>)
      (<:expr< (fun f x l -> List.fold_left (fun acc e -> f e acc) x l) >>);
    rule_dict (list (tuple [v"x"; v"y"])) (v"x") (v"y") (<:expr< [] >>)
      (<:expr< (fun k v l -> (k, v) :: l) >>)
      (<:expr< (fun f x l -> List.fold_left (fun acc (k, v) -> f k v acc)) >>);
  ]

exception Cannot_generate

let rec generate_reader rules dbust (camlt : mono) =
  let gr, gw = generate_reader rules, generate_writer rules in
  let rules = List.map (fun r -> fst (r gr gw)) rules in
    match Gen.generate rules (term_of_dbus_type dbust) camlt with
      | Some(v) -> v
      | None -> raise Cannot_generate

and generate_writer rules (camlt : mono) dbust =
  let gr, gw = generate_reader rules, generate_writer rules in
  let rules = List.map (fun r -> snd (r gr gw)) rules in
    match Gen.generate rules (term_of_dbus_type dbust) camlt with
      | Some(v) -> v
      | None -> raise Cannot_generate

let rec default_type : DBus.typ -> mono = function
  | DBus.Tbyte -> char
  | DBus.Tboolean -> bool
  | DBus.Tint16 -> int
  | DBus.Tuint16 -> int
  | DBus.Tint32 -> int
  | DBus.Tuint32 -> int
  | DBus.Tint64 -> int64
  | DBus.Tuint64 -> int64
  | DBus.Tdouble -> float
  | DBus.Tstring -> string
  | DBus.Tobject_path -> string
  | DBus.Tsignature -> dbus_type
  | DBus.Tarray(x) -> list (default_type x)
  | DBus.Tdict(k, v) -> list (tuple [default_type k; default_type v])
  | DBus.Tstruct(l) -> tuple (List.map default_type l)
  | DBus.Tvariant -> dbus_value

let parse_module_sigs f = Tree.empty

let correct_module_name = String.capitalize
let camlify name = Util.ljoin "_" (Util.split_upper name)
let correct_signal_name = camlify
let correct_method_name = camlify

let with_labels = ref false

let args = [
  ("-label", Arg.Set with_labels, "use labels")
]

module Print (File : sig val ch_mli : out_channel end) =
struct
  let print fmt = Printf.fprintf File.ch_mli fmt

  let print_seq null b e sep f = function
    | [] -> print null
    | [t] -> f t
    | t :: l ->
        print b;
        f t;
        List.iter (fun t -> print sep; f t) l;
        print e

  let print_mult f = function
    | [] -> ()
    | [e] -> f e
    | e :: l -> f e; List.iter (fun e -> print "\n"; f e) l

  let rec list_of_tuple : mono -> mono list = function
    | `RTerm("*nil", []) -> []
    | `RTerm("*cons", [t; tl]) -> t :: list_of_tuple tl
    | _ -> assert false

  let rec print_type : mono -> unit = function
    | `RTerm("*nil", []) -> print "unit"
    | `RTerm("*cons", _) as t -> print_seq "unit" "(" ")" " * " print_type (list_of_tuple t)
    | `RTerm(t, args) ->
        print_seq "" "(" ")" ", " print_type args;
        if args <> [] then print " ";
        print "%s" t

  let rec print_module indent name (Tree.Node(i, sons)) =
    let defs = (match i with None -> [] | Some(x) -> x) in
      print "%smodule %s : sig\n" indent name;
      if defs <> [] then
        print "%s  type t

%s  val proxy : OBus.proxy -> t

" indent indent;
      print_mult begin function
        | Interface.Method(name, ins, outs) ->
            let in_names, in_types = List.split ins
            and out_names, out_types = List.split outs in
              print "%s  val %s : t -> " indent name;
              begin match ins with
                | [] -> print "unit -> "
                | _ -> List.iter (fun (name, typ) ->
                                    if !with_labels then begin
                                      print "~(%s:" name;
                                      print_type typ;
                                      print ")"
                                    end else
                                      print_type typ;
                                    print " -> ") ins
              end;
              print_type (tuple out_types);
              print "\n%s    (** [%s" indent name;
              print_seq " ()" "" "" "" (print " %s") in_names;
              print "] result: ";
              print_seq "()" "" "" ", " (print "%s") out_names;
              print " *)\n"
        | Interface.Signal(name, args) ->
            let arg_names, arg_types = List.split args in
              print "%s  val %s : " indent name;
              print_type (t1 "signal" (tuple arg_types));
              print "\n%s    (** args: " indent;
              print_seq "()" "" "" ", " (print "%s") arg_names;
              print " *)\n"
      end defs;
      if defs <> [] && sons <> [] then print "\n";
      print_modules (indent ^ "  ") sons;
      print "%send\n" indent
  and print_modules indent l =
    print_mult (fun (n, m) -> print_module indent n m) l

  let print_file fname l =
    print "\
(*
 * %s
 * %s
 *
 * File generated by %s.
 *)

" fname (String.make (String.length fname) '-') (Filename.basename (Sys.argv.(0)));
    print_modules "" l
end

let write_module_sigs fname (Tree.Node(_, l)) =
  Util.with_open_out fname begin fun ch_mli ->
    let module P = Print(struct
                           let ch_mli = ch_mli
                         end) in
      P.print_file fname l
  end

let write_module_strs fname mstr = ()
