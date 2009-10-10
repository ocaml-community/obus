(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open Ocamlbuild_plugin

(* Syntax extensions used internally, (tag and the byte-code file). *)
let intern_syntaxes = [ "pa_obus", "pa_obus.cma";
                        "pa_projection", "syntax/pa_projection.cmo";
                        "pa_constructor", "syntax/pa_constructor.cmo";
                        "pa_log", "syntax/pa_log.cmo";
                        "pa_monad", "syntax/pa_monad.cmo" ]

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

let packages = [ "type-conv";
                 "type-conv.syntax";
                 "camlp4";
                 "camlp4.extend";
                 "camlp4.lib";
                 "camlp4.macro";
                 "camlp4.quotations.o";
                 "camlp4.quotations.r";
                 "lwt";
                 "lwt.unix";
                 "lwt.syntax";
                 "str";
                 "xml-light";
                 "react" ]

let syntaxes = [ "camlp4o";
                 "camlp4r" ]

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "doc"; tag] f

let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

let define_lib ?dir name =
  ocaml_lib ?dir name;
  dep ["ocaml"; "byte"; "use_" ^ name] [name ^ ".cma"];
  dep ["ocaml"; "native"; "use_" ^ name] [name ^ ".cmxa"]

let substitute env text =
  List.fold_left (fun text (patt, repl) -> String.subst patt repl text) text env

let get_public_modules () =
  List.filter (fun s -> not (String.is_prefix "src/private/" s)) (string_list_of_file "obus.mllib")

let get_version () =
  match string_list_of_file "VERSION" with
    | version :: _ -> version
    | _ -> failwith "invalid VERSION file"

let _ =
  dispatch begin function
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        let ocamlfind x = S[A"ocamlfind"; A x] in
        Options.ocamlc   := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        Options.ocamldoc := ocamlfind "ocamldoc"

    | After_rules ->
        (* Tests must see everything *)
        Pathname.define_context "test" [ "src"; "src/private" ];

        (* The library and internal modules can see each other *)
        Pathname.define_context "src" [ "src/private" ];
        Pathname.define_context "src/private" [ "src" ];

        (* The syntax extension need to see the library because it use
           some of its modules *)
        Pathname.define_context "syntax" [ "src" ];

        (* +---------------------------------------------------------+
           | Libraries                                               |
           +---------------------------------------------------------+ *)

        define_lib ~dir:"src" "obus";
        define_lib ~dir:"bindings/hal" "hal";
        define_lib ~dir:"bindings/notification" "notification";

        (* +---------------------------------------------------------+
           | Shared libraries                                        |
           +---------------------------------------------------------+ *)

        rule "shared libraries (cmxs)"
          ~dep:"%.cmxa" ~prod:"%.cmxs"
          (fun env _ -> Cmd(S[!(Options.ocamlopt); A"-shared"; A"-linkall"; A(env "%.cmxa"); A"-o"; A(env "%.cmxs")]));

        (* +---------------------------------------------------------+
           | Ocamlfind stuff                                         |
           +---------------------------------------------------------+ *)

        (* When one link an OCaml binary, one should use -linkpkg *)
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option
           when compiling, computing dependencies, generating
           documentation and linking. *)
        List.iter
          (fun package -> flag_all_stages ("pkg_" ^ package) (S[A"-package"; A package]))
          packages;

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
          syntaxes;

        (* +---------------------------------------------------------+
           | Internal syntaxes                                       |
           +---------------------------------------------------------+ *)

        List.iter
          (fun (tag, file) ->
             flag_all_stages_except_link tag & S[A"-ppopt"; A file];
             dep ["ocaml"; "ocamldep"; tag] [file])
          intern_syntaxes;

        (* +---------------------------------------------------------+
           | Other                                                   |
           +---------------------------------------------------------+ *)

        (* Generation of the OBus_version.ml file *)
        rule "version" ~prod:"src/private/OBus_version.ml" ~dep:"VERSION"
          (fun _ _ -> Echo(["let version = \"" ^ get_version () ^ "\"\n"], "src/private/OBus_version.ml"));

        (* Generation of the obus.odocl file *)
        rule "obus_doc" ~prod:"obus.odocl" ~dep:"obus.mllib"
          (fun _ _ -> Echo(List.map (fun s -> s ^ "\n") (get_public_modules ()), "obus.odocl"));

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "obus.mllib"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())] (read_file "META.in")], "META"))

    | _ -> ()
  end
