open Printf
open Ocamlbuild_plugin
open Command (* no longer needed for OCaml >= 3.10.2 *)

let prepend dir fnames = List.map (fun fname -> dir / fname) fnames

module Config =
struct
  (* Tell weather to use C stubs for low-level serialization *)
  let use_c_stubs = false

  (* Modules shared by all libraries *)
  let common_modules =
    ["common/constant";
     "common/util"]

  (* Modules shared by obus and obus_thread *)
  let obus_modules =
    ["obus/authLexer";
     "obus/addrLexer";
     "obus/OBus"]

  let obus_thread_module = "obus/with_threads/threadImplem"
  let obus_no_thread_module = "obus/without_threads/threadImplem"

  (* Sub-modules of the OBus module *)
  let obus_pack_files =
    ["types";
     "info";
     "path";
     "wire";
     "message";
     "values";
     "interface";
     "address";
     "transport";
     "wireMessage";
     "auth";
     "error";
     "connection";
     "cookie";
     "proxy";
     "rules";
     "bus";
     "signal";
     "DBus"]

  (* Hidden sub-modules (must not appears in documentation) *)
  let hidden = [ "wire"; "wireMessage" ]

  let syntaxes = ["pa_log"; "pa_seq"; "trace"]
end

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

(* this lists all supported packages *)
let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
    run_and_read "ocamlfind list | cut -d' ' -f1"

(* this is supposed to list available syntaxes, but I don't know how to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

let intern_lib dir lib =
  ocaml_lib ~dir:dir lib;
  dep ["ocaml"; "byte"; "use_" ^ lib] [lib ^ ".cma"];
  dep ["ocaml"; "native"; "use_" ^ lib] [lib ^ ".cmxa"]

let _ =
  List.iter
    (fun fname ->
       tag_file (sprintf "obus/%s.cmx" fname) ["for-pack(OBus)"])
    Config.obus_pack_files;

  dispatch begin function
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        Options.ocamlc   := ocamlfind & A"ocamlc";
        Options.ocamlopt := ocamlfind & A"ocamlopt";
        Options.ocamldep := ocamlfind & A"ocamldep";
        Options.ocamldoc := ocamlfind & A"ocamldoc"

    | After_rules ->
        Pathname.define_context "obus/with_threads" [ "obus/threadsigs" ];
        Pathname.define_context "obus/without_threads" [ "obus/threadsigs" ];
        Pathname.define_context "obus" [ "obus/threadsigs"; "common" ];
        Pathname.define_context "binder" [ "common" ];
        Pathname.define_context "tools" [ "common"; "binder"; "obus"; "interfaces" ];
        Pathname.define_context "samples" [ "interfaces" ];
        Pathname.define_context "samples/threaded" [ "interfaces" ];
        Pathname.define_context "interfaces" [ "obus" ];
        Pathname.define_context "test" [ "common"; "binder" ];

        (* rule for building dbus interface binding *)
        rule "obus-binding"
          ~prods:["%.ml"; "%.mli"]
          ~deps:["%.xml"; "tools/obus-binder.byte"]
          (fun env builder ->
             let xml = env "%.xml" in
               Cmd(S[P"./tools/obus-binder.byte"; A xml; A "-o"; A (env "%"); T(tags_of_pathname xml++"obus-binder"++"generate")]));
        flag ["obus-binder"; "generate"; "internal"] & A"-internal";

        (* When one link an OCaml library/binary/package, one should use -linkpkg *)
        flag ["ocaml"; "link"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option when
         * compiling, computing dependencies, generating documentation and
         * linking. *)
        List.iter begin fun pkg ->
          flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
          flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
          flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
          flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
        end (find_packages ());

        (* Like -package but for extensions syntax. Morover -syntax is useless
         * when linking. *)
        List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
        end (find_syntaxes ());

        List.iter begin fun tag ->
          flag ["ocaml"; "pp"; tag] & A("syntax/" ^ tag ^ ".cmo");
          dep ["ocaml"; "ocamldep"; tag] ["syntax/" ^ tag ^ ".cmo"]
        end Config.syntaxes;

        (* Internal libraries *)
        intern_lib "obus" "obus";
        intern_lib "obus" "obus-thread";



        rule "obus_pack"
          ~prod:"obus/OBus.mlpack"
          (fun _ _ -> Echo(List.map (sprintf "%a\n" (fun _ -> module_name_of_pathname)) Config.obus_pack_files,
                           "obus/OBus.mlpack"));

        rule "obus_mli_to_install"
          ~prod:"lib-dist"
          (fun _ _ -> Echo(List.map (sprintf "obus/%s.mli\n")
                             (List.filter
                                (fun s -> not (List.mem s Config.hidden))
                                Config.obus_pack_files),
                           "lib-dist"));

        rule "obus_doc"
          ~prod:"obus.odocl"
          (fun _ _ -> Echo(List.map (sprintf "obus/%a\n" (fun _ -> module_name_of_pathname))
                             (List.filter
                                (fun s -> not (List.mem s Config.hidden))
                                Config.obus_pack_files),
                           "obus.odocl"));

        let library_rule filename additional_modules =
          rule filename
            ~prod:filename
            ~deps:[]
            (fun _ _ -> Echo(List.map (fun s -> s ^ "\n")
                               (Config.common_modules
                                @ additional_modules
                                @ Config.obus_modules),
                             filename)) in

          library_rule "obus.mllib"
            [Config.obus_no_thread_module];
          library_rule "obus-thread.mllib"
            [Config.obus_thread_module];
    | _ -> ()
  end
