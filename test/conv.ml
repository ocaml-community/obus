(*
 * conv.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open GenSerializer
open Types
open Helpers
open Optimize

let foo = typ "foo" []
let bar = typ "bar" []

let rules =
  rule_convert foo (tuple [int; int])
    (<:expr< fun (x,y) -> Foo(x, y) >>)
    (<:expr< function Foo(x, y) -> (x, y) >>)
  :: rule_record bar
    [F"a", int;
     F"b", foo;
     F"c", string]
  :: default_rules

let test env a b =
  Printf.printf "testing: %s <-> %s => %!"
    (string_of_type a)
    (signature_of_dbus_type b);
  try
    let result = gen_reader false rules a b env in
      Printf.printf "OK\n";
      Some result
  with
      Failure _ ->
        Printf.printf "Failure\nHere is the trace:\n%!";
        begin try
          ignore (gen_reader true rules a b []);
        with _ -> ()
        end;
        print_newline ();
        None

module Printer = Camlp4.Printers.OCaml.Make(Syntax)

let tests =
  [int, [Tuint32];
   list int, [Tarray Tint16];
   list (tuple [int; int]), [Tarray (Tstructure [Tuint32; Tbyte])];
   tuple [int; int; int], [Tint32; Tuint32; Tint32];
   tuple [bool; list (tuple [string; string])], [Tboolean; Tdict(Tstring, Tstring)];
   tuple [tuple [int; int]], [Tint32; Tuint32];
   tuple [int; tuple [int; int; int]; int], [Tint32; Tint32; Tuint32; Tuint32; Tint32];
   bar, [Tint32; Tuint32; Tbyte; Tstring]]

let _ =
  let env, funcs =
    List.fold_left
      (fun (env, funcs) (a, b) ->
         match test env a b with
           | None -> (env, funcs)
           | Some(env, code) ->
               let opt = optimize false 0 8 code in
               let expr = GenCode.generate_reader false Env.empty opt.opt_code
                 (fun env -> Ast.exCom_of_list (List.map expr_of_id (Env.all env))) in
                 (env, (a, b, expr) :: funcs))
      ([], []) tests in

  let printer = new Printer.printer () in
  let oc = open_out "/tmp/result.ml" in
  let formatter = Format.formatter_of_out_channel oc in
    List.iter (fun (id, expr) ->
                 printer#str_item formatter
                   (<:str_item<
                      let $id:id$ = $expr$
                        >>);
                 Format.pp_print_newline formatter ();
                 Format.pp_print_newline formatter ()) env;
    List.iter (fun (a, b, expr) ->
                 Printf.fprintf oc "(* reading function for: %s -> %s *)\n%!"
                   (signature_of_dbus_type b)
                   (string_of_type a);
                 printer#str_item formatter
                   (<:str_item<
                      let reader buffer i = $expr$
                        >>);
                 Format.pp_print_newline formatter ();
                 Format.pp_print_newline formatter ()) (List.rev funcs);
    close_out oc;
    Printf.printf "result saved in /tmp/result.ml\n"
