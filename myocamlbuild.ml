(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         | After_rules ->
             (* Internal syntax extension *)
             flag ["ocaml"; "compile"; "pa_obus"] & S[A"-ppopt"; A"syntax/pa_obus.cmo"];
             flag ["ocaml"; "ocamldep"; "pa_obus"] & S[A"-ppopt"; A"syntax/pa_obus.cmo"];
             flag ["ocaml"; "doc"; "pa_obus"] & S[A"-ppopt"; A"syntax/pa_obus.cmo"];
             dep ["ocaml"; "ocamldep"; "pa_obus"] ["syntax/pa_obus.cmo"];

             (* Generation of ocaml modules from .obus files *)
             rule "IDL to OCaml"
               ~prods:["%.ml"; "%.mli"]
               ~deps:["%.obus"; "tools/obus_gen_interface.byte"]
               (fun env _ -> Cmd(S[P"tools/obus_gen_interface.byte";
                                   A"-keep-common";
                                   A"-o"; A(env "%");
                                   A(env "%.obus")]));

             (* Generation of ocaml modules from xml introspection files *)
             rule "XML to OCaml"
               ~prods:["%.ml"; "%.mli"]
               ~deps:["%.xml"; "tools/obus_gen_interface.byte"]
               (fun env _ -> Cmd(S[P"tools/obus_gen_interface.byte"; A"-o"; A(env "%"); A(env "%.xml")]));

             (* Use an introduction page with categories *)
             tag_file "obus-api.docdir/index.html" ["apiref"];
             dep ["apiref"] ["apiref-intro"];
             flag ["apiref"] & S[A "-intro"; P "apiref-intro"; A"-colorize-code"]

         | _ ->
             ())

