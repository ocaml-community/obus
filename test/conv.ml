(*
 * conv.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Types
open Solver
open Instruction
open GenSerializer

let _loc = Loc.ghost

let foo = typ "foo" []
let bar = typ "bar" []

let common_rules =
  [ rule_record bar
      ["a", int;
       "b", foo;
       "c", string] ]

let rrules =
  (function
     | Type("foo", []), x ->
         dep [< (tuple [int; int], x) >]
           (fun is -> flat is @ [Iconvert(<:expr< fun (x, y) -> Foo(x, y) >>)])
     | _ -> fail)
  :: common_rules
  @ Reading.default_rules

let wrules =
  (function
     | Type("foo", []), x ->
         dep [< (tuple [int; int], x) >]
           (fun is -> [Iconvert(<:expr< fun (Foo(x, y)) -> (x, y) >>)] @ flat is)
     | _ -> fail)
  :: common_rules
  @ Writing.default_rules

let test eqn =
  Printf.printf "testing: %s => %!" (string_of_eqn eqn);
  match solve rrules eqn with
    | Some sol ->
        Printf.printf "OK\n"
    | None  ->
        Printf.printf "Failure\nHere is the trace:\n%!";
        ignore (solve ~printer:string_of_eqn rrules eqn);
        print_newline ()

module Printer = Camlp4.Printers.OCaml.Make(Syntax)

let tests =
  [int, Tsingle Tuint32;
   list int, Tseq [Tarray Tint16];
   list (tuple [int; int]), Tseq [Tarray (Tstructure [Tuint32; Tbyte])];
   tuple [int; int; int], Tseq [Tint32; Tuint32; Tint32];
   tuple [bool; list (tuple [string; string])], Tseq [Tboolean; Tdict(Tstring, Tstring)];
   tuple [bool; list (tuple [string; string])], Tseq [Tboolean; Tdict(Tstring, Tstring)];
   tuple [tuple [int; int]], Tseq [Tint32; Tuint32];
   tuple [int; tuple [int; int; int]; int], Tseq [Tint32; Tint32; Tuint32; Tuint32; Tint32];
   tuple [int; foo; string], Tsingle (Tstructure [Tuint32; Tint32; Tuint32; Tstring]);
   bar, Tsingle (Tstructure  [Tint32; Tbyte; Tuint32; Tstring])]

let _ =
  List.iter test tests;

  let renv =
    List.fold_left
      (fun env instrs ->
         let expr, env = Compile.compile_reader instrs Ast.exCom_of_list env in
           snd (Compile.lookup expr env))
      Compile.empty_env
      (Util.filter_map (solve rrules) tests)

  and wenv =
    List.fold_left
      (fun env instrs ->
         let vars, expr, env = Compile.compile_writer instrs <:expr< i >> env in
           snd (Compile.lookup (List.fold_right (fun x e -> <:expr< fun $x$ -> $e$ >>) vars expr) env))
      Compile.empty_env
      (Util.filter_map (solve wrules) tests)

  in

  let printer = new Printer.printer () in
  let oc = open_out "/tmp/result.ml" in
  let formatter = Format.formatter_of_out_channel oc in
  let print_sols env =
    List.iter (fun (id, expr) ->
                 printer#str_item formatter
                   (<:str_item<
                      let $id:id$ = $expr$
                        >>);
                 Format.pp_print_newline formatter ();
                 Format.pp_print_newline formatter ()) (Compile.dump_env env)
  in
    print_sols renv;
    print_sols wenv;
    close_out oc;
    Printf.printf "result saved in /tmp/result.ml\n"
