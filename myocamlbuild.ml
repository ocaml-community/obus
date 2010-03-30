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
    Command.execute ~quiet:true (Cmd(S[Sh command; Sh"> /dev/null"; Sh"2> /dev/null"]));
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
(*  "network_state";*)
]

let bindings = [
  "hal";
  "notification";
(*  "NetworkManager";*)
  "upower";
]

let tools = [
  "obus_introspect";
  "obus_binder";
  "obus_dump";
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
  "lwt.react";
  "lwt.text";
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

let substitute resolv text =
  let buf = Buffer.create (String.length text) in
  Buffer.add_substitute buf resolv text;
  Buffer.contents buf

let version = lazy(
  match string_list_of_file "VERSION" with
    | version :: _ -> version
    | _ -> failwith "invalid VERSION file"
)

let _ =
  dispatch begin function
    | Before_options ->

        Options.make_links := false;

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
        Pathname.define_context "tests" [ "src"; "src/private" ];

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

        (* Choose between native and byte-code for compiling
           binaries *)
        if have_native then
          rule "best" ~dep:"%.native" ~prod:"%.best"
            (fun env _ -> ln_s (Filename.basename (env "%.native")) (env "%.best"))
        else
          rule "best" ~dep:"%.byte" ~prod:"%.best"
            (fun env _ -> ln_s (Filename.basename (env "%.byte")) (env "%.best"));

        let libs_byte =
          "obus.cma" :: List.map (fun name -> "bindings" / name / name ^ ".cma") bindings
        and libs_native = List.concat [
          "obus.cmxa" :: List.map (fun name -> "bindings" / name / name ^ ".cmxa") bindings;
          "obus.cmxs" :: List.map (fun name -> "bindings" / name / name ^ ".cmxs") bindings;
        ]
        and libs_debug =
          "obus.d.cma" :: List.map (fun name -> "bindings" / name / name ^ ".d.cma") bindings
        and bins_byte = List.concat [
          List.map (sprintf "examples/%s.byte") examples;
          List.map (sprintf "tools/%s.byte") tools;
        ]
        and bins_native = List.concat [
          List.map (sprintf "examples/%s.native") examples;
          List.map (sprintf "tools/%s.native") tools;
        ]
        and bins_debug = List.concat [
          List.map (sprintf "examples/%s.d.byte") examples;
          List.map (sprintf "tools/%s.d.byte") tools;
        ]
        and bins_best = List.concat [
          List.map (sprintf "examples/%s.best") examples;
          List.map (sprintf "tools/%s.best") tools;
        ]
        and common = List.concat [
          ["pa_obus.cma"; "META"; "doc"];
          (* Man pages for tools: *)
          List.map (fun t -> sprintf "man/%s.1.gz" (String.subst "_" "-" t)) tools
        ] in

        virtual_rule "all" & List.concat [
          common;
          libs_byte;
          if have_native then libs_native else [];
          bins_best;
        ];
        virtual_rule "byte" & List.concat [
          common;
          libs_byte;
          bins_byte;
        ];
        virtual_rule "native" & List.concat [
          common;
          libs_native;
          bins_native;
        ];
        virtual_rule "debug" & List.concat [
          common;
          libs_debug;
          bins_debug;
        ];
        virtual_rule "libs" & List.concat [
          ["pa_obus.cma"; "META"];
          libs_byte;
          if have_native then libs_native else [];
        ];

        (* +---------------------------------------------------------+
           | Libraries                                               |
           +---------------------------------------------------------+ *)

        ocaml_lib ~dir:"src" "obus";
        dep ["ocaml"; "byte"; "use_obus"] ["obus.cma"];
        dep ["ocaml"; "native"; "use_obus"] ["obus.cmxa"];

        List.iter
          (fun name ->
             ocaml_lib ~dir:("bindings" / name) ("bindings" / name / name);
             dep ["ocaml"; "byte"; "use_" ^ name] ["bindings" / name / name ^ ".cma"];
             dep ["ocaml"; "native"; "use_" ^ name] ["bindings" / name / name ^ ".cmxa"])
          bindings;

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
          (fun _ _ -> Echo([sprintf "let version = %S\n" (Lazy.force version)], "src/private/OBus_version.ml"));

        (* Generation of the obus.odocl file *)
        let mllibs = "obus.mllib" :: List.map (fun name -> "bindings" / name / name ^ ".mllib") bindings in
        rule "obus doc" ~prod:"obus.odocl" ~deps:mllibs
          (fun _ _ -> Echo(List.map (sprintf "%s\n")
                             (List.concat
                                (List.filter (fun module_name -> Filename.dirname module_name = "src") (string_list_of_file "obus.mllib")
                                 :: List.map
                                 (fun name ->
                                    List.map
                                      (fun module_name -> "bindings" / name / module_name)
                                      (string_list_of_file ("bindings" / name / name ^ ".mllib")))
                                 bindings)),
                           "obus.odocl"));

        (* Use an introduction page with categories *)
        dep ["file:obus.docdir/index.html"] ["utils/doc/apiref-intro"];
        flag ["file:obus.docdir/index.html"] & S[A"-intro"; P"utils/doc/apiref-intro"; A"-colorize-code"];

        (* Build documentation then copy our css to the documentation
           directory *)
        rule "Documentation with custom css" ~deps:["utils/doc/style.css"; "obus.docdir/html.stamp"] ~stamp:"doc"
          (fun _ _ -> cp "utils/doc/style.css" "obus.docdir/style.css");

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "obus.mllib"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute
                     (function
                        | "version" -> Lazy.force version
                        | name -> ksprintf failwith "unknown variable %s" name)
                     (read_file "META.in")],
                  "META"));

        (* Compress manual pages *)
        rule "gzip" ~dep:"%" ~prod:"%.gz"
          (fun env _ -> Cmd(S[A"gzip"; A"-c"; A(env "%"); Sh">"; A(env "%.gz")]))

    | _ ->
        ()
  end
