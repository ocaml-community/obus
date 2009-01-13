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
open Command (* no longer needed for OCaml >= 3.10.2 *)

(* Syntax extensions used internally *)
let intern_syntaxes = ["trace"; "pa_obus"]

(* +-----------------------------------+
   | Packages installed with ocamlfind |
   +-----------------------------------+ *)

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let exec cmd =
  blank_sep_strings &
    Lexing.from_string &
    run_and_read cmd

(* this lists all supported packages *)
let find_packages () = exec "ocamlfind list | cut -d' ' -f1"

(* this is supposed to list available syntaxes, but I don't know how
   to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

(* +-------+
   | Utils |
   +-------+ *)

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

let _ =
  dispatch begin function
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        Options.ocamlc   := ocamlfind & A"ocamlc";
        Options.ocamlopt := ocamlfind & A"ocamlopt";
        Options.ocamldep := ocamlfind & A"ocamldep";
        Options.ocamldoc := ocamlfind & A"ocamldoc"

    | After_rules ->
        (* Tests must see everything *)
        Pathname.define_context "test" [ "obus" ];
        Pathname.define_context "test" [ "obus/internals" ];

        (* The library and internal modules can see each other *)
        Pathname.define_context "obus" [ "obus/internals" ];
        Pathname.define_context "obus/internals" [ "obus" ];

        (* +-----------------------+
           | Dependencies checking |
           +-----------------------+ *)

        if not (Pathname.exists "_build/dependencies-checked") then
          execute ~quiet:true (Cmd(Sh"/bin/sh check-deps.sh > /dev/null"));

        (* +---------------------+
           | Manual dependencies |
           +---------------------+ *)

        dep ["file:syntax/pa_obus.ml"] ["name_translator.ml"];
        dep ["file:obus/oBus_info.ml"] ["version.ml"];

        (* +-----------+
           | Libraries |
           +-----------+ *)

        define_lib ~dir:"obus" "obus";
        define_lib ~dir:"bindings/hal" "hal";
        define_lib ~dir:"bindings/notification" "notification";

        (* +------------------+
           | Shared libraries |
           +------------------+ *)

        rule "shared libraries (cmxs)"
          ~dep:"%.cmxa" ~prod:"%.cmxs"
          (fun env _ -> Cmd(S[!(Options.ocamlopt); A"-shared"; A"-linkall"; A(env "%.cmxa"); A"-o"; A(env "%.cmxs")]));

        (* +-----------------+
           | Ocamlfind stuff |
           +-----------------+ *)

        (* When one link an OCaml library/binary/package, one should use -linkpkg *)
        flag ["ocaml"; "link"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option
           when compiling, computing dependencies, generating
           documentation and linking. *)
        List.iter
          (fun pkg -> flag_all_stages ("pkg_" ^ pkg) (S[A"-package"; A pkg]))
          (find_packages ());

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
          (find_syntaxes ());

        (* +-------------------+
           | Internal syntaxes |
           +-------------------+ *)

        List.iter
          (fun tag ->
             flag_all_stages_except_link tag & S[A"-ppopt"; A("syntax/" ^ tag ^ ".cmo")];
             dep ["ocaml"; "ocamldep"; tag] ["syntax/" ^ tag ^ ".cmo"])
          intern_syntaxes;

        (* +-------+
           | Other |
           +-------+ *)

        (* Generation of the version.ml file *)
        rule "version" ~prod:"version.ml" ~dep:"VERSION"
          (fun _ _ -> match string_list_of_file "VERSION" with
             | version :: _ -> Echo(["let version = \"" ^ version ^ "\"\n"], "version.ml")
             | _ -> failwith "invalid VERSION file");

        (* Generation of the obus.odocl file *)
        rule "obus_doc" ~prod:"obus.odocl" ~dep:"obus.mllib"
          (fun _ _ -> Echo(List.map (fun s -> s ^ "\n")
                             (List.filter (fun s -> not (String.is_prefix "obus/internals/" s))
                                (string_list_of_file "obus.mllib")),
                           "obus.odocl"))

    | _ -> ()
  end
