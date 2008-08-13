open Printf
open Ocamlbuild_plugin
open Command (* no longer needed for OCaml >= 3.10.2 *)

type binding_desc = {
  name : string;
  desc : string;
  modules : string list;
}

module Config =
struct
  (***** Library configuration *****)

  let obus_version = "0.1"

  (* All the modules of the obus library *)
  let all_modules =
    [ "Addr_lexer";
      "Auth_lexer";
      "Util";
      "OBus_info";
      "Types_rw";
      "OBus_path";
      "OBus_value";
      "Wire";
      "OBus_xml_parser";
      "OBus_introspect";
      "OBus_address";
      "OBus_transport";
      "OBus_auth";
      "OBus_header";
      "OBus_internals";
      "Wire_message";
      "OBus_error";
      "OBus_connection";
      "OBus_type";
      "OBus_proxy";
      "OBus_client";
      "OBus_bus" ]

  (* Internal modules, which are not be part of the API *)
  let hidden_modules =
    [ "Wire_message";
      "Addr_lexer";
      "Auth_lexer";
      "Util";
      "Wire";
      "Types_rw";
      "OBus_internals" ]

  (* Modules of the API *)
  let modules = List.filter (fun s -> not & List.mem s hidden_modules) all_modules

  (***** Bindings *****)

  let bindings =
    [ { name = "hal";
        desc = "Hal service binding";
        modules = ["Hal_device"; "Hal_manager"] };
      { name = "notify";
        desc = "Notifications service binding";
        modules = ["Notify"] } ]

  (***** Generation of the META file *****)

  let meta () =
    let buf = Buffer.create 512 in
    bprintf buf "
description = \"Pure OCaml implementation of DBus\"
version = \"%s\"
browse_interfaces = \"%s\"
requires = \"lwt\"
archive(byte) = \"obus.cma\"
archive(native) = \"obus.cmxa\"
package \"syntax\" (
  version = \"[distrubuted with OBus]\"
  description = \"Syntactic sugar for OBus: DBus types written as caml types + convertion function\"
  requires = \"camlp4\"
  archive(syntax,preprocessor) = \"pa_obus.cmo\"
  archive(syntax,toploop) = \"pa_obus.cmo\"
)\n" obus_version (String.concat " " modules);
    List.iter begin fun b ->
      bprintf buf "package \"%s\" (
  version = \"[distrubuted with OBus]\"
  description = \"%s\"
  browse_interfaces = \"%s\"
  requires = \"obus\"
  archives(byte) = \"%s.cma\"
  archives(native) = \"%s.cmxa\"
)\n" b.name b.desc (String.concat " " b.modules) b.name b.name
    end bindings;
    Buffer.contents buf

  (* Syntax extensions used internally *)
  let intern_syntaxes = ["pa_log"; "trace"; "pa_obus"]
end

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let exec cmd =
  blank_sep_strings &
    Lexing.from_string &
    run_and_read cmd

(* this lists all supported packages *)
let find_packages () = exec "ocamlfind list | cut -d' ' -f1"

(* this is supposed to list available syntaxes, but I don't know how to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

let _ =
  dispatch begin function
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        Options.ocamlc   := ocamlfind & A"ocamlc";
        Options.ocamlopt := ocamlfind & A"ocamlopt";
        Options.ocamldep := ocamlfind & A"ocamldep";
        Options.ocamldoc := ocamlfind & A"ocamldoc"

    | After_rules ->
        Pathname.define_context "test" [ "obus" ];

        rule "obus_lib" ~prod:"obus.mllib"
          (fun _ _ -> Echo(List.map (sprintf "obus/%s\n") Config.all_modules, "obus.mllib"));
        ocaml_lib ~dir:"obus" "obus";
        dep ["ocaml"; "byte"; "use_obus"] ["obus.cma"];
        dep ["ocaml"; "native"; "use_obus"] ["obus.cmxa"];

        List.iter begin fun { name = name; modules = modules } ->
          rule (name ^ "_lib") ~prod:(name ^ ".mllib")
            (fun _ _ -> Echo(List.map (sprintf "bindings/%s/%s\n" name) modules,
                             name ^ ".mllib"));
          ocaml_lib ~dir:(sprintf "bindings/%s" name) name;
          dep ["ocaml"; "byte"; "use_" ^ name] [name ^ ".cma"];
          dep ["ocaml"; "native"; "use_" ^ name] [name ^ ".cmxa"]
        end Config.bindings;

        rule "META" ~prod:"META"
          (fun _ _ -> Echo([Config.meta ()], "META"));

        rule "obus_doc" ~prod:"obus.odocl"
          (fun _ _ -> Echo(List.map (sprintf "obus/%s\n") Config.modules, "obus.odocl"));

        rule "mli_to_install" ~prod:"lib-dist"
          (fun _ _ -> Echo(List.map (fun s -> sprintf "obus/%s.mli\n" (String.uncapitalize s)) Config.modules, "lib-dist"));

        rule "obus_version" ~prod:"obus/version.ml"
          (fun _ _ -> Echo([sprintf "let version = %S\n" Config.obus_version], "obus/version.ml"));

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
          flag ["ocaml"; "compile"; tag] & S[A"-ppopt"; A("syntax/" ^ tag ^ ".cmo")];
          flag ["ocaml"; "ocamldep"; tag] & S[A"-ppopt"; A("syntax/" ^ tag ^ ".cmo")];
          flag ["ocaml"; "doc"; tag] & S[A"-ppopt"; A("syntax/" ^ tag ^ ".cmo")];
          dep ["ocaml"; "ocamldep"; tag] ["syntax/" ^ tag ^ ".cmo"]
        end Config.intern_syntaxes;

    | _ -> ()
  end
