(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open Ocamlbuild_plugin

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

let try_exec command =
  try
    let _ = run_and_read command in
    true
  with _ ->
    false

let () =
  if not (try_exec "ocamlfind printconf") then begin
    prerr_endline "ocamlfind is not available, please install it";
    exit 1
  end

let have_native = try_exec "ocamlfind ocamlopt -version"

let examples = [
  "hello";
  "bus_functions";
  "eject";
  "notify";
  "monitor";
  "signals";
  "list_services";
  "ping";
  "pong";
]

let bindings = [
  "hal";
  "notification";
]

let libs = "obus" :: bindings

let tools = [
  "obus_introspect";
  "obus_binder";
  "obus_dump";
]

let tests = [
  "test_serialization";
  "test_printing";
  "test_communication";
  "test_valid";
  "test_auth";
  "test_server";
  "test_errors";
]

(* Syntax extensions used internally, (tag and the byte-code file). *)
let intern_syntaxes = [
  "pa_obus", "pa_obus.cma";
  "pa_projection", "syntax/pa_projection.cmo";
  "pa_constructor", "syntax/pa_constructor.cmo";
  "pa_monad", "syntax/pa_monad.cmo";
]

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

let packages = [
  "type-conv";
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
  "lwt.syntax.log";
  "str";
  "xmlm";
  "react";
]

let syntaxes = [
  "camlp4o";
  "camlp4r";
]

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

let flag_all_stages_except_link tags f =
  flag ("ocaml" :: "compile" :: tags) f;
  flag ("ocaml" :: "ocamldep" :: tags) f;
  flag ("ocaml" :: "doc" :: tags) f

let flag_all_stages tags f =
  flag_all_stages_except_link tags f;
  flag ("ocaml" :: "link" :: tags) f

let define_lib ?dir name =
  ocaml_lib ?dir name;
  dep ["ocaml"; "byte"; "use_" ^ name] [name ^ ".cma"];
  dep ["ocaml"; "native"; "use_" ^ name] [name ^ ".cmxa"]

let substitute env text =
  List.fold_left (fun text (patt, repl) -> String.subst patt repl text) text env

let get_public_modules () =
  List.filter (fun s -> not (String.is_prefix "src/private/" s) && String.is_prefix "src/" s)
    (string_list_of_file "obus.mllib")

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
        (* FIXME: sometimes ocamldoc say that elements are not found
           even if they are present: *)
        Options.ocamldoc := S[A"ocamlfind"; A"ocamldoc"; A"-hide-warnings"]

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
           | Virtual targets                                         |
           +---------------------------------------------------------+ *)

        let virtual_rule name deps =
          rule name ~stamp:name ~deps (fun _ _ -> Nop)
        in

        if have_native then
          rule "best" ~dep:"%.native" ~prod:"%.best"
            (fun env _ -> cp (env "%.native") (env "%.best"))
        else
          rule "best" ~dep:"%.byte" ~prod:"%.best"
            (fun env _ -> cp (env "%.byte") (env "%.best"));

        let byte = "syntax/pa_obus.cma" :: List.concat [
          List.map (sprintf "%s.cma") libs;
          List.map (sprintf "examples/%s.byte") examples;
          List.map (sprintf "tools/%s.byte") tools;
        ]
        and native = List.concat [
          List.map (sprintf "%s.cmxa") libs;
          List.map (sprintf "%s.cmxs") libs;
          List.map (sprintf "examples/%s.native") examples;
          List.map (sprintf "tools/%s.native") tools;
        ]
        and debug = List.concat [
          List.map (sprintf "%s.d.cma") libs;
          List.map (sprintf "examples/%s.d.byte") examples;
          List.map (sprintf "tools/%s.d.byte") tools;
        ]
        and common = "META" :: "obus.docdir/index.html" :: List.map (fun t -> sprintf "man/%s.1.gz" (String.subst "_" "-" t)) tools in

        virtual_rule "all" & byte @ (if have_native then native else []) @ List.map (sprintf "tools/%s.best") tools @ common;
        virtual_rule "byte" & byte @ common;
        virtual_rule "native" & native @ common;
        virtual_rule "debug" & debug @ common;
        virtual_rule "tests" & (List.map (sprintf "test/%s.d.byte") tests);

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
          (fun package -> flag_all_stages ["pkg_" ^ package] (S[A"-package"; A package]))
          packages;

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ["syntax_" ^ syntax] (S[A"-syntax"; A syntax]))
          syntaxes;

        (* +---------------------------------------------------------+
           | Internal syntaxes                                       |
           +---------------------------------------------------------+ *)

        List.iter
          (fun (tag, file) ->
             flag_all_stages_except_link [tag] & S[A"-ppopt"; A file];
             dep ["ocaml"; "ocamldep"; tag] [file])
          intern_syntaxes;

        (* +---------------------------------------------------------+
           | Other                                                   |
           +---------------------------------------------------------+ *)

        (* Keep debugging message if compiling in debug mode *)
        flag_all_stages_except_link ["pkg_lwt.syntax.log"; "debug"] & S[A"-ppopt"; A"-lwt-debug"];

        (* Generation of the OBus_version.ml file *)
        rule "version" ~prod:"src/private/OBus_version.ml" ~dep:"VERSION"
          (fun _ _ -> Echo(["let version = \"" ^ get_version () ^ "\"\n"], "src/private/OBus_version.ml"));

        (* Generation of the obus.odocl file *)
        rule "obus_doc" ~prod:"obus.odocl" ~dep:"obus.mllib"
          (fun _ _ -> Echo(List.map (fun s -> s ^ "\n") (get_public_modules ()), "obus.odocl"));

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "obus.mllib"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())] (read_file "META.in")], "META"));

        rule "gzip" ~dep:"%" ~prod:"%.gz"
          (fun env _ -> Cmd(S[A"gzip"; A"-c"; A(env "%"); Sh">"; A(env "%.gz")]))
    | _ -> ()
  end
