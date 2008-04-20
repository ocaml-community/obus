(*
 * caml.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
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

module Gen = Generate.Make
  (struct
     type var = string
     type left = dbus_type
     type right = string
   end)
  (struct
     type t = expr
   end)

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make
          (Camlp4.OCamlInitSyntax.Make(Ast)(Gram)(Quotation))));;

let _loc = Loc.ghost

module Rules =
struct
  type typ =
    [ `RTerm of string * typ list
    | `Var of string ]

  let t0 id = `RTerm(id, [])
  let t1 id a0 = `RTerm(id, [a0])
  let t2 id a0 a1 = `RTerm(id, [a0; a1])
  let t3 id a0 a1 a2 = `RTerm(id, [a0; a1; a2])

  let mkt id args = `RTerm(id, args)

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
  let dbus_value = t0 "OBus.Values.single"
  let dbus_type = t0 "OBus.Types.t"
  let tuple l = match l with
    | [] -> t0 "unit"
    | [t] -> t
    | _ :: _ :: _ -> List.fold_right (fun t acc -> t2 "*cons" t acc) l (t0 "*nil")

  type rule_desc =
    | Array of typ * expr * expr * expr
    | Dict of typ * typ * expr * expr * expr
    | Record of (string * typ) list
    | Any of typ * expr * expr
  type convertion_rule = typ * rule_desc

  let map key_type mod_name =
    (t1 (mod_name ^ ".t") (v"x"),
     Dict(key_type, v"x",
          <:expr< $uid:mod_name$ . empty >>,
          <:expr< $uid:mod_name$ . add >>,
          <:expr< $uid:mod_name$ . fold >>))

  let default =
    [ (int, Any(char, <:expr< int_of_char >>, <:expr< char_of_int >>));
      (list (v"x"),
       Array(v"x",
             <:expr< [] >>, <:expr< fun x l -> x :: l >>,
             <:expr< (fun f x l -> List.fold_left (fun acc e -> f e acc) x l) >>));
      (list (tuple [v"x"; v"y"]),
       Dict(v"x", v"y",
            <:expr< [] >>, <:expr< (fun k v l -> (k, v) :: l) >>,
            <:expr< (fun f x l -> List.fold_left (fun acc (k, v) -> f k v acc) x l) >>)) ]
end

module Make(R : sig val rules : Rules.convertion_rule list end) =
struct
  let name = "caml"

  open Rules

  type var = [ `Var of string ]
  type 'a app = [ `RTerm of string * 'a list ]
  type pat = Gen.rpattern
  type typ = Gen.rterm

  let string_of_seq null b e sep = function
    | [] -> null
    | [t] -> t
    | t :: l ->
        b ^ t ^ List.fold_left (fun acc t -> acc ^ sep ^ t) "" l ^ e

  let rec list_of_tuple : typ -> typ list = function
    | `RTerm("*nil", []) -> []
    | `RTerm("*cons", [t; tl]) -> t :: list_of_tuple tl
    | _ -> assert false

  let rec string_of_type : typ -> string = function
    | `RTerm("*nil", []) ->
        "unit"
    | `RTerm("*cons", _) as t ->
        string_of_seq "unit" "(" ")" " * " (List.map string_of_type (list_of_tuple t))
    | `RTerm(t, args) ->
        string_of_seq "" "(" ")" ", " (List.map string_of_type args) ^ (if args <> [] then " " else "") ^ t

  let type_of_ctyp str typ =
    let fail () = raise (Invalid_argument ("can not understand this caml type: " ^ str)) in
    let parse_id id =
      Util.ljoin "." (List.map (function
                                  | Ast.IdLid(_, t) -> t
                                  | Ast.IdUid(_, t) -> t
                                  | _ -> fail ()) (Ast.list_of_ident id [])) in
    let rec parse_app acc = function
      | (<:ctyp< $id:t$ >>) -> (List.rev acc, parse_id t)
      | (<:ctyp< $a$ $b$ >>) -> parse_app (parse_type a :: acc) b
      | _ -> fail ()
    and parse_tuple : Ast.ctyp -> typ list = function
      | Ast.TySta(_, a, b) -> parse_tuple a @ parse_tuple b
      | t -> [parse_type t]
    and parse_type t : typ = match t with
      | Ast.TyTup(_, t) -> tuple (parse_tuple t)
      | _ -> let args, id = parse_app [] t in `RTerm(id, args)
    in
      parse_type typ

  let type_of_string str : typ =
    type_of_ctyp str (Caml.Gram.parse Caml.ctyp _loc (Stream.of_string str))

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

  let make_rule left right reader writer =
    (Gen.make_generator left right reader,
     Gen.make_generator left right writer)

  let rule_any (ctyp : pat) (ctyp_from : pat) read write =
    let rtype = ctyp_from
    and rvars = [< ctyp_from >]
    and rreader reader = <:expr< fun i -> let i, v = $reader$ i in (i, $read$ v) >>
    and rwriter writer = <:expr< fun v i -> $writer$ ($write$ v) i >> in
      make_rule (rtype : pat :> Gen.pattern) ctyp (Gen.Seq(rvars, rreader)) (Gen.Seq(rvars, rwriter))

  let rule_array (ctyp : pat) (etyp : pat) empty add fold =
    let rtype = darray (etyp : pat :> Gen.pattern)
    and rvars = [< etyp >]
    and rreader ereader =
      (<:expr< R.read_array $ereader$ $empty$ $add$ >>)
    and rwriter ewriter = <:expr< W.write_array $ewriter$ $fold$ >> in
      make_rule rtype ctyp (Gen.Seq(rvars, rreader)) (Gen.Seq(rvars, rwriter))

  let rule_dict (ctyp : pat) (ktyp : pat) (vtyp : pat) empty add fold =
    let rtype = ddict (ktyp : pat :> Gen.pattern) (vtyp : pat :> Gen.pattern)
    and rvars = [< ktyp; vtyp >]
    and rreader kreader vreader = <:expr< R.read_dict $kreader$ $vreader$ $empty$ $add$ >>
    and rwriter kwriter vwriter = <:expr< W.write_dict $kwriter$ $vwriter$ $fold$ >> in
      make_rule rtype ctyp (Gen.Seq(rvars, rreader)) (Gen.Seq(rvars, rwriter))

  let rule_record (ctyp : pat) (fields : (string * pat) list) =
    let rtype = List.fold_right (fun (_, ctyp) acc -> dcons (ctyp : pat :> Gen.pattern) acc) fields dnil
    and rvars = List.map snd fields;
    and rreader l =
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
    and rwriter l =
      <:expr< (fun v i ->
                 $(List.fold_right2 begin fun (name, _) writer acc ->
                     <:expr<
                       let i = $writer$ (v . $lid:name$) i in
                         $acc$ >>
                   end fields l <:expr< i >>)$)
      >> in
      make_rule rtype ctyp (Gen.List(rvars, rreader)) (Gen.List(rvars, rwriter))

  let rule_basic (ctyp : typ) (dtyp : Gen.lterm) reader writer =
    let rtype = (dtyp : Gen.lterm :> Gen.pattern)
    and rvars = [<>]
    and rreader = <:expr< R . $lid:reader$ >>
    and rwriter = <:expr< W . $lid:writer$ >> in
      make_rule rtype (ctyp : typ :> pat) (Gen.Seq(rvars, rreader)) (Gen.Seq(rvars, rwriter))

  let rec rules = List.split begin List.map begin fun (t, desc) -> match desc with
    | Array(et, empty, add, fold) -> rule_array t et empty add fold
    | Dict(kt, vt, empty, add, fold) -> rule_dict t kt vt empty add fold
    | Record(fields) -> rule_record t fields
    | Any(st, read, write) -> rule_any t st read write
  end R.rules
    @ [ (let vars = [< (v"x") >] in
           make_rule (dstruct (v"x")) (v"x")
             (Gen.Seq(vars, fun r -> <:expr< read_struct $r$ >>))
             (Gen.Seq(vars, fun w -> <:expr< write_struct $w$ >>)));
        (let vars = [< (v"x"); (v"y") >] in
           make_rule (dcons (v"x") (v"y")) (t2 "*cons" (v"x") (v"y"))
             (Gen.Seq(vars, fun rx ry -> <:expr< fun i -> let i, vx = $rx$ i in let i, vy = $ry$ i in (i, (vx, vy)) >>))
             (Gen.Seq(vars, fun wx wy -> <:expr< fun (vx, vy) i -> let i = $wx$ vx i in let i = $wy$ vy i in i >>)));
        (let vars = Seq.nil in
           make_rule dnil (t0 "*nil")
             (Gen.Seq(vars, <:expr< fun i -> (i, ()) >>))
             (Gen.Seq(vars, <:expr< fun () i -> i >>)));
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
        rule_basic bool dboolean "read_boolean" "write_boolean";
        rule_basic float ddouble "read_double" "write_double";
        rule_basic string dstring "read_string" "write_string";
        rule_basic dbus_type dsignature "read_signature" "write_signature";
        rule_basic dbus_value dvariant "read_variant" "write_variant";
        rule_basic string dsignature "read_signature_as_string" "write_signature_from_string";
        rule_basic string dobject_path "read_object_path" "write_object_path" ]
  end
  and rule_variant (ctyp : pat) (variants : (int * string * DBus.typ * typ) list) =
    let rtype : Gen.pattern = dcons (int : typ :> Gen.pattern) (dcons dvariant dnil)
    and rvars = [< (int : typ :> pat) >]
    and rreader int_reader =
      let match_expr =
        Ast.ExMat(_loc,
                  Ast.ExId(_loc, Ast.IdLid(_loc, "chooser")),
                  List.fold_left begin fun expr (n, cstr, dbust, paramt) ->
                    Ast.McOr(_loc,
                             Ast.McArr(_loc,
                                       Ast.PaInt(_loc, string_of_int n),
                                       Ast.ExNil _loc,
                                       let reader = generate_reader dbust paramt in
                                         <:expr< let i, v =
                                           R.read_fixed_variant $str:DBus.string_of_type dbust$ $reader$ i in
                                           (i, $uid:cstr$(v)) >>),
                             expr)
                  end (Ast.McNil _loc) variants) in
        <:expr< fun i ->
          let i, chooser = $int_reader$ i in
            $match_expr$ >>
    and rwriter int_writer =
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
                                       let writer = generate_writer paramt dbust in
                                         <:expr< let i = $int_writer$ $int:string_of_int n$ i in
                                           W.write_fixed_variant $str:DBus.string_of_type dbust$ $writer$ x i >>),
                             expr)
                  end (Ast.McNil _loc) variants) in
        <:expr< fun i -> $match_expr$ >> in
      make_rule rtype ctyp (Gen.Seq(rvars, rreader)) (Gen.Seq(rvars, rwriter))

  and generate_reader dbust (camlt : typ) =
    match Gen.generate (fst rules) (term_of_dbus_type dbust) camlt with
      | Some(v) -> v
      | None -> failwith
          (Printf.sprintf "can not generate a reader for : %s -> %s"
             (DBus.string_of_type dbust)
             (string_of_type camlt))

  and generate_writer (camlt : typ) dbust =
    match Gen.generate (snd rules) (term_of_dbus_type dbust) camlt with
      | Some(v) -> v
      | None -> failwith
          (Printf.sprintf "can not generate a writer for : %s -> %s"
             (string_of_type camlt)
             (DBus.string_of_type dbust))

  let rec default_type : DBus.typ -> typ = function
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

  let correct_module_name = String.capitalize
  let camlify name = Util.ljoin "_" (Util.split_upper name)
  let correct_signal_name = camlify
  let correct_method_name = camlify
  let correct_arg_name = function
    | "type" -> "typ"
    | x -> x

  let with_labels = ref false

  let args_mapper = []
  let args_generator = [
    ("-label", Arg.Set with_labels, "use labels")
  ]

  module PrintSig (File : sig val ch_mli : out_channel end) =
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

    let rec list_of_tuple : typ -> typ list = function
      | `RTerm("*nil", []) -> []
      | `RTerm("*cons", [t; tl]) -> t :: list_of_tuple tl
      | _ -> assert false

    let rec print_type : typ -> unit = function
      | `RTerm("*nil", []) -> print "unit"
      | `RTerm("*cons", _) as t -> print_seq "unit" "(" ")" " * " print_type (list_of_tuple t)
      | `RTerm(t, args) ->
          print_seq "" "(" ")" ", " print_type args;
          if args <> [] then print " ";
          print "%s" t

    let split_args args =
      List.split (List.map (fun (Sig.Arg(n, t)) -> (n, t)) args)

    let rec print_module indent (Sig.Sig(name, defs), sons) =
      print "%smodule %s : sig\n" indent name;
      if defs <> [] then
        print "%s  type t

%s  val proxy : OBus.bus -> string -> string -> t

" indent indent;
      print_mult begin function
        | Sig.Method(name, ins, outs) ->
            let in_names, in_types = split_args ins
            and out_names, out_types = split_args outs in
              print "%s  val %s : t -> " indent name;
              begin match ins with
                | [] -> print "unit -> "
                | _ -> List.iter (fun (Sig.Arg(name, typ)) ->
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
        | Sig.Signal(name, args) ->
            ()
            (*let arg_names, arg_types = split_args args in
              print "%s  val %s : " indent name;
              print_type (t1 "Obus.signal" (tuple arg_types));
              print "\n%s    (** args: " indent;
              print_seq "()" "" "" ", " (print "%s") arg_names;
              print " *)\n"*)
      end defs;
      if defs <> [] && sons <> Sig.empty then print "\n";
      print_modules (indent ^ "  ") sons;
      print "%send\n" indent
    and print_modules indent (Sig.Tree(l)) =
      print_mult (print_module indent) l

    let print_sig fname mapping =
      print "\
(*
 * %s
 * %s
 *
 * File generated by %s.
 *)

" fname (String.make (String.length fname) '-') (Filename.basename (Sys.argv.(0)));
      print_modules "" mapping.Lmap.sigs
  end

  module GenExpr =
  struct
    open Sig

    let sig_of_args args =
      List.fold_left (fun acc (Arg(_, dtype)) -> acc ^ DBus.string_of_type dtype) "" args

    let make_def interface_name ddef ldef = match ddef, ldef with
      | (Method(dname, dins, douts),
         Method(lname, lins, louts)) ->
          let writer = List.fold_right2 begin fun (Arg(_, dtype)) (Arg(lname, ltype)) acc ->
            <:expr< let i = $generate_writer ltype dtype$ $lid:lname$ i in $acc$ >>
          end dins lins (<:expr< i >>) in
          let reader = List.fold_right2 begin fun (Arg(_, dtype)) (Arg(lname, ltype)) acc ->
            <:expr< let i, $lid:lname$ = $generate_reader dtype ltype$ i in $acc$ >>
          end douts louts
            (let result_expr = match louts with
               | [] -> <:expr< () >>
               | _ -> Ast.exCom_of_list (List.map (fun (Arg(name, _)) -> <:expr< $lid:name$ >>) louts)
             in <:expr< (i, $result_expr$) >>)
          in
          let send_sig = sig_of_args dins
          and repl_sig = sig_of_args douts in
          let body =
            (<:expr< OBus.LowLevel.send_message bus (Header.Signature $str:send_sig$ :: Header.Member $str:dname$ :: fields)
               $str:send_sig$ $str:repl_sig$
               (fun byte_order buffer i -> match byte_order with
                  | Header.LittleEndian ->
                      let module W = OBus.LowLevel.Writer(OBus.LowLevel.LittleEndian)(struct let buffer = buffer end)
                      in $writer$
                  | Header.BigEndian ->
                      let module W = OBus.LowLevel.Writer(OBus.LowLevel.BigEndian)(struct let buffer = buffer end)
                      in $writer$)
               (fun byte_order buffer i -> match byte_order with
                  | Header.LittleEndian ->
                      let module R = OBus.LowLevel.Reader(OBus.LowLevel.LittleEndian)(struct let buffer = buffer end)
                      in $reader$
                  | Header.BigEndian ->
                      let module R = OBus.LowLevel.Reader(OBus.LowLevel.BigEndian)(struct let buffer = buffer end)
                      in $reader$) >>)
          in
          let f_expr = match lins with
            | [] -> (<:expr< fun () -> $body$ >>)
            | _ -> List.fold_right (fun (Arg(name, _)) acc -> <:expr< fun $lid:name$ -> $acc$ >>) lins body
          in
            (<:str_item< let $lid:lname$ (bus, fields) = $f_expr$ >>)
      | (Signal(_, _),
         Signal(lname, _)) -> (<:str_item< let $lid:lname$ = Obj.magic 0 >>)
      | _ -> assert false

    let rec make_mod map lnames (Sig(lname, ldefs), Tree sons) =
      let lnames = lname :: lnames in
      let content = begin match ldefs with
        | [] -> <:str_item< >>
        | _ ->
            let (Sig(dname, ddefs)) = List.assoc (Util.rjoin "." lnames) map in
              (<:str_item< type t = OBus.bus * Header.fields list ;;
                           let proxy bus dest path = (bus, [Header.Destination dest; Header.Path path; Header.Interface $str:dname$]) ;;
                           $Ast.stSem_of_list (List.map2 (make_def dname) ddefs ldefs)$ >>)
      end in
      let son_mods = List.map (make_mod map lnames) sons in
        (<:str_item< module $uid:lname$ =
                     struct
                       $content$ ;;
                       $Ast.stSem_of_list son_mods$
                     end >>)

    let main mapping =
      let (Tree mods) = mapping.Lmap.sigs in
        Ast.stSem_of_list (List.map (make_mod mapping.Lmap.map []) mods)
  end

  let generate fname mapping =
    let mli_name = fname ^ ".mli" in
    let ml_name = fname ^ ".ml" in
      Util.with_open_out mli_name
        begin fun ch_mli ->
          let module P = PrintSig(struct
                                    let ch_mli = ch_mli
                                  end) in
            P.print_sig mli_name mapping
        end;
      Caml.print_implem ~output_file:ml_name (GenExpr.main mapping)
end
