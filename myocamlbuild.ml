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

(* Syntax extensions used internally, (tag and the byte-code file). *)
let intern_syntaxes = ["pa_trace", "syntax/trace.cmo";
                       "pa_obus", "pa_obus.cma";
                       "pa_projection", "syntax/pa_projection.cmo";
                       "pa_constructor", "syntax/pa_constructor.cmo"]

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

(* List of packages that must appears in a certain order. Sometimes
   there are problems with syntax extensions... *)
let packages_needing_ordering = [ "type-conv.syntax"; "camlp4.macro" ]

(* Extract the list of used packages from "_tags" *)
let get_packages () =
  List.fold_left
    (fun set word ->
       if String.is_prefix "pkg_" word then begin
         let start = String.length "pkg_"
         and stop =
           if String.is_suffix word "," then
             String.length word - 1
           else
             String.length word
         in
         let pkg = String.sub word start (stop - start) in
         if List.mem pkg packages_needing_ordering then
           set
         else
           StringSet.add pkg set
       end else
         set) StringSet.empty (string_list_of_file "_tags")

(* List of syntaxes *)
let syntaxes = ["camlp4o"; "camlp4r"]

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

(* For each ocamlfind package one inject the -package option when
   compiling, computing dependencies, generating documentation and
   linking. *)
let define_package pkg =
  flag_all_stages ("pkg_" ^ pkg) & S[A"-package"; A pkg]

let substitute env text =
  List.fold_left (fun text (patt, repl) -> String.subst patt repl text) text env

let get_public_modules _ =
  List.filter (fun s -> not (String.is_prefix "obus/internals/" s)) (string_list_of_file "obus.mllib")

let get_version _ =
  match string_list_of_file "VERSION" with
    | version :: _ -> version
    | _ -> failwith "invalid VERSION file"

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

        (* The syntax extension need to see the library because it use
           some of its modules *)
        Pathname.define_context "syntax" [ "obus" ];

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

        (* Deals with packages needing ordering first *)
        List.iter define_package packages_needing_ordering;

        (* Add others in any order *)
        StringSet.iter define_package (get_packages ());

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
          syntaxes;

        (* +-------------------+
           | Internal syntaxes |
           +-------------------+ *)

        List.iter
          (fun (tag, file) ->
             flag_all_stages_except_link tag & S[A"-ppopt"; A file];
             dep ["ocaml"; "ocamldep"; tag] [file])
          intern_syntaxes;

        (* +-------+
           | Other |
           +-------+ *)

        (* Generation of the version.ml file *)
        rule "version" ~prod:"version.ml" ~dep:"VERSION"
          (fun _ _ -> Echo(["let version = \"" ^ get_version () ^ "\"\n"], "version.ml"));

        (* Generation of the obus.odocl file *)
        rule "obus_doc" ~prod:"obus.odocl" ~dep:"obus.mllib"
          (fun _ _ -> Echo(List.map (fun s -> s ^ "\n") (get_public_modules ()), "obus.odocl"));

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "obus.mllib"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ());
                               ("@MODULES@", String.concat " "
                                  (List.map
                                     (fun s -> String.after s (String.length "obus/"))
                                     (get_public_modules ())))]
                     (read_file "META.in")], "META"))

    | _ -> ()
  end
