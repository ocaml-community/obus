(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Ocamlbuild_plugin

let try_exec command =
  try
    Command.execute ~quiet:true (Cmd(S[Sh command; Sh"> /dev/null"; Sh"2> /dev/null"]));
    true
  with _ ->
    false

let have_native = try_exec "ocamlfind ocamlopt -version"

let () =
  dispatch
    (function
       | Before_options ->

           Options.use_ocamlfind := true

       | After_rules ->

           if have_native then
             rule "best" ~dep:"%.native" ~prod:"%.best"
               (fun env _ -> ln_s (Filename.basename (env "%.native")) (env "%.best"))
           else
             rule "best" ~dep:"%.byte" ~prod:"%.best"
               (fun env _ -> ln_s (Filename.basename (env "%.byte")) (env "%.best"));

           rule "meltpp" ~dep:"%.mlt" ~prod:"%.ml"
             (fun env _ -> Cmd(S[A"meltpp"; A(env "%.mlt"); A"-o"; A(env "%.ml")]));

           rule "manual" ~dep:"manual.best" ~prod:"manual.tex"
             (fun env _ -> Cmd(P"./manual.best"));

           rule "manual-colored" ~dep:"manual.best" ~prod:"manual-colored.tex"
             (fun env _ -> Cmd(S[P"./manual.best"; A"-use-colors"]));

           rule "latex" ~dep:"%.tex" ~prod:"%.pdf"
             (fun env _ -> Seq[Cmd(S[A"xelatex"; A(env "%.tex")]);
                               Cmd(S[A"xelatex"; A(env "%.tex")])])

       | _ ->
           ())
