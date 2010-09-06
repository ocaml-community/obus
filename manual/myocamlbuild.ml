(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Ocamlbuild_plugin

let () =
  dispatch
    (function
       | Before_options ->

           Options.use_ocamlfind := true

       | After_rules ->

           tag_any ["package(melt)"; "package(camlp4.lib)"];

           rule "meltpp" ~dep:"%.mlt" ~prod:"%.ml"
             (fun env _ -> Cmd(S[A"meltpp"; A(env "%.mlt"); A"-o"; A(env "%.ml")]));

           let codes = List.map (fun file -> "codes" / file) (Array.to_list (Sys.readdir "codes")) in

           rule "manual" ~deps:("manual.byte" :: codes)  ~prod:"manual.tex"
             (fun env _ -> Cmd(P"./manual.byte"));

           rule "manual-colored" ~deps:("manual.byte" :: codes) ~prod:"manual-colored.tex"
             (fun env _ -> Cmd(S[P"./manual.byte"; A"-use-colors"]));

           rule "latex" ~dep:"%.tex" ~prod:"%.pdf"
             (fun env _ -> Seq[Cmd(S[A"xelatex"; A(env "%.tex")]);
                               Cmd(S[A"xelatex"; A(env "%.tex")])])

       | _ ->
           ())
