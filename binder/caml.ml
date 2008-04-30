(*
 * caml.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open DBus

type expr = Ast.expr

type caml_id = string
type 'a caml_type = (caml_id, 'a) Type.term

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make
          (Camlp4.OCamlInitSyntax.Make(Ast)(Gram)(Quotation))));;

let _loc = Loc.ghost

open Type

module Rules =
struct
  let t0 id = Type(id, [])
  let t1 id a0 = Type(id, [a0])
  let int = t0 "int"
  let int32 = t0 "int32"
  let int64 = t0 "int64"
  let float = t0 "float"
  let bool = t0 "bool"
  let char = t0 "char"
  let string = t0 "string"
  let list x = t1 "list" x
  let array x = t1 "array" x
  let dbus_value = t0 "OBus.Values.value"
  let dbus_types = t0 "OBus.Values.dtypes"

  type rule_desc =
    | Array of var caml_type * expr * expr * expr
    | Dict of var caml_type * var caml_type * expr * expr * expr
    | Record of (string * var caml_type) list
    | Any of var caml_type * expr * expr
  type convertion_rule = var caml_type * rule_desc

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
  type id = caml_id
  type typ = id Type.typ

  open Rules

  let intern = section ()
  let v x = Var(intern, x)

  let string_of_seq null b e sep = function
    | [] -> null
    | [t] -> t
    | t :: l ->
        b ^ t ^ List.fold_left (fun acc t -> acc ^ sep ^ t) "" l ^ e

  let rec string_of_type : none caml_type -> string = function
    | Nil ->
        "unit"
    | Cons _ as t ->
        string_of_seq "unit" "(" ")" " * " (List.map string_of_type (list_of_tuple t))
    | Type(t, args) ->
        string_of_seq "" "(" ")" ", " (List.map string_of_type args) ^ (if args <> [] then " " else "") ^ t
    | Var _ -> assert false

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
    and parse_type t = match t with
      | Ast.TyTup(_, t) -> tuple (List.map parse_type (Ast.list_of_ctyp t []))
      | _ -> let args, id = parse_app [] t in Type(id, args)
    in
      parse_type typ

  let type_of_string str =
    type_of_ctyp str (Caml.Gram.parse Caml.ctyp _loc (Stream.of_string str))

  module Term =
  struct
    type left = dbus_id
    type right = caml_id
  end

  let flatr = function
    | [] -> <:expr< () >>
    | [x] -> x
    | l ->
        let names = Util.gen_names "v" l in
          List.fold_right2 begin fun n e expr ->
            <:expr< let i, $lid:n$ = $e$ i in $expr$ >>
          end names l (Ast.exCom_of_list (List.map (fun x -> <:expr< $lid:x$ >>) names))

  let flatw = function
    | [] -> <:expr< () >>
    | [x] -> x
    | l ->
        let names = Util.gen_names "v" l in
          <:expr< fun $Ast.paCom_of_list (List.map (fun x -> <:patt< $lid:x$ >>) names)$ ->
            $List.fold_right2 begin fun n e expr ->
              <:expr< let i = $e$ i $lid:n$ in $expr$ >>
            end names l <:expr< i >>$ >>

  module GenR = Generate.Make(Term)
    (struct
       type t = expr
       let flat = flatr
     end)

  module GenW = Generate.Make(Term)
    (struct
       type t = expr
       let flat = flatw
     end)

  let make_rule left right args rest reader writer =
    GenR.add_rule left right args rest reader;
    GenW.add_rule left right args rest writer

  let rule_any ctyp ctyp_from read write =
    let lpat = v"x"
    and args = [< (v"x", ctyp_from) >]
    and rreader reader _ = [ <:expr< fun i -> let i, v = $flatr reader$ i in (i, $read$ v) >> ]
    and rwriter writer _ = [ <:expr< fun v i -> $flatw writer$ ($write$ v) i >> ] in
      make_rule lpat ctyp args [] rreader rwriter

  let rule_array ctyp etyp empty add fold =
    let lpat = darray (v"x")
    and args = [< (v"x", etyp) >]
    and rreader ereader _ = [ <:expr< R.read_array $flatr ereader$ $empty$ $add$ >> ]
    and rwriter ewriter _ = [ <:expr< W.write_array $flatw ewriter$ $fold$ >> ] in
      make_rule lpat ctyp args [] rreader rwriter

  let rule_dict ctyp ktyp vtyp empty add fold =
    let lpat = ddict (v"k") (v"v")
    and args = [< (v"k", ktyp); (v"v", vtyp) >]
    and rreader kreader vreader _ = [ <:expr< R.read_dict $flatr kreader$ $flatr vreader$ $empty$ $add$ >> ]
    and rwriter kwriter vwriter _ = [ <:expr< W.write_dict $flatw kwriter$ $flatw vwriter$ $fold$ >> ] in
      make_rule lpat ctyp args [] rreader rwriter

  let rule_record ctyp fields =
    let names = Util.gen_names "" fields in
    let lpat = tuple (List.map v names)
    and rest = List.map2 (fun n (_, ctyp) -> (v n, ctyp)) names fields
    and rreader l =
      [ let l = List.map flatr l in
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
                                          Ast.ExNil _loc))))) ]
    and rwriter l =
      [ let l = List.map flatw l in
      <:expr< (fun v i ->
                 $(List.fold_right2 begin fun (name, _) writer acc ->
                     <:expr<
                       let i = $writer$ (v . $lid:name$) i in
                         $acc$ >>
                   end fields l <:expr< i >>)$)
      >> ] in
      make_rule lpat ctyp [<>] rest rreader rwriter

  let rule_basic ctyp dtyp reader writer =
    let rreader _ = [ <:expr< R . $lid:reader$ >> ]
    and rwriter _ = [ <:expr< W . $lid:writer$ >> ] in
      make_rule dtyp ctyp [<>] [] rreader rwriter

  let _ =
    List.iter begin fun (t, desc) -> match desc with
      | Array(et, empty, add, fold) -> rule_array t et empty add fold
      | Dict(kt, vt, empty, add, fold) -> rule_dict t kt vt empty add fold
      | Record(fields) -> rule_record t fields
      | Any(st, read, write) -> rule_any t st read write
    end R.rules;
    make_rule (dstructure (v"x")) (v"x") [< (v"x", v"x") >] []
      (fun r _ -> [ <:expr< read_struct $flatr r$ >> ])
      (fun w _ -> [ <:expr< write_struct $flatw w$ >> ]);
    rule_basic int dint16 "int" "int16";
    rule_basic int duint16 "int" "uint16";
    rule_basic int dint32 "int" "int32";
    rule_basic int duint32 "int" "uint32";
    rule_basic int dint64 "int" "int64";
    rule_basic int duint64 "int" "uint64";
    rule_basic int32 dint32 "int32" "int32";
    rule_basic int32 duint32 "int32" "uint32";
    rule_basic int64 dint64 "int64" "int64";
    rule_basic int64 duint64 "int64" "uint64";
    rule_basic char dbyte "char" "byte";
    rule_basic bool dboolean "bool" "boolean";
    rule_basic float ddouble "float" "double";
    rule_basic string dstring "string" "string";
    rule_basic dbus_types dsignature "read_signature" "write_signature";
    rule_basic dbus_value dvariant "read_variant" "write_variant";
    rule_basic string dsignature "read_signature_as_string" "write_signature_from_string";
    rule_basic string dobject_path "read_object_path" "write_object_path"

(*  let rule_variant (ctyp : pat) (variants : (int * string * DBus.typ * typ) list) =
    let lpat : Gen.pattern = dcons (int : typ :> Gen.pattern) (dcons dvariant dnil)
    and args = [< (int : typ :> pat) >]
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
      make_rule lpat ctyp (Gen.Seq(args, rreader)) (Gen.Seq(args, rwriter))
*)

  and generate_reader dbust camlt =
    match GenR.generate (DBus.dbus_type_of_dtypes dbust) camlt with
      | Some(v) -> v
      | None -> failwith
          (Printf.sprintf "can not generate a reader for : %s -> %s"
             (DBus.signature_of_dtypes dbust)
             (string_of_type camlt))

  and generate_writer camlt dbust =
    match GenW.generate (DBus.dbus_type_of_dtypes dbust) camlt with
      | Some(v) -> v
      | None -> failwith
          (Printf.sprintf "can not generate a writer for : %s -> %s"
             (string_of_type camlt)
             (DBus.signature_of_dtypes dbust))

  let default_type t =
    let rec aux = function
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
      | DBus.Tsignature -> dbus_types
      | DBus.Tarray(x) -> list (aux x)
      | DBus.Tdict(k, v) -> list (tuple [aux k; aux v])
      | DBus.Tstructure(l) -> tuple (List.map aux l)
      | DBus.Tvariant -> dbus_value
    in tuple (List.map aux t)

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

    let rec print_type : none caml_type -> unit = function
      | Nil -> print "unit"
      | Cons _ as t -> print_seq "unit" "(" ")" " * " print_type (list_of_tuple t)
      | Type(t, args) ->
          print_seq "" "(" ")" ", " print_type args;
          if args <> [] then print " ";
          print "%s" t
      | Var _ -> assert false

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
      List.fold_left (fun acc (Arg(_, dtype)) -> acc ^ DBus.signature_of_dtypes dtype) "" args

    let make_def interface_name ddef ldef = match ddef, ldef with
      | (Method(dname, dins, douts),
         Method(lname, lins, louts)) ->
          let writer = List.fold_right2 begin fun (Arg(_, dtype)) (Arg(lname, ltype)) acc ->
            <:expr< let i = $flatw (generate_writer ltype dtype)$ $lid:lname$ i in $acc$ >>
          end dins lins (<:expr< i >>) in
          let reader = List.fold_right2 begin fun (Arg(_, dtype)) (Arg(lname, ltype)) acc ->
            <:expr< let i, $lid:lname$ = $flatr (generate_reader dtype ltype)$ i in $acc$ >>
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
