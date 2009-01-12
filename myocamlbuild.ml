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

type binding_desc = {
  name : string;
  desc : string;
  modules : string list;
}

module Config =
struct
  (***** Library configuration *****)

  let obus_version = "0.3"

  (* All the modules of the obus library *)
  let all_modules =
    [ "internals/Constant";
      "internals/MQueue";
      "internals/Log";
      "internals/Addr_lexer";
      "internals/Util";
      "internals/MSet";
      "internals/Cache";
      "OBus_string";
      "OBus_info";
      "internals/Types_rw";
      "OBus_path";
      "OBus_value";
      "OBus_type";
      "OBus_name";
      "OBus_uuid";
      "internals/Match_rule";
      "internals/Xparser";
      "OBus_address";
      "OBus_lowlevel";
      "OBus_auth";
      "OBus_message";
      "internals/OBus_internals";
      "OBus_error";
      "OBus_introspect";
      "OBus_connection";
      "internals/Bus";
      "OBus_peer";
      "OBus_proxy";
      "OBus_resolver";
      "OBus_signal";
      "OBus_property";
      "OBus_interface";
      "OBus_bus";
      "OBus_object";
      "OBus_server"]

  (* Modules of the API *)
  let modules = List.filter (fun s -> not (String.is_prefix "internals/" s)) all_modules

  (***** Bindings *****)

  let bindings =
    [ { name = "hal";
        desc = "Hal service binding";
        modules = ["Hal"] };
      { name = "notify";
        desc = "Notifications service binding";
        modules = ["Notify"] };
      { name = "avahi";
        desc = "Avahi binding";
        modules = ["Avahi"] } ]

  (***** Generation of the META file *****)

  let meta () =
    let buf = Buffer.create 512 in
    bprintf buf "
description = \"Pure OCaml implementation of DBus\"
version = \"%s\"
browse_interfaces = \"%s\"
requires = \"lwt xml-light\"
archive(byte) = \"obus.cma\"
archive(native) = \"obus.cmxa\"
package \"syntax\" (
  version = \"[distrubuted with OBus]\"
  description = \"Syntactic sugars for OBus\"
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
  archive(byte) = \"%s.cma\"
  archive(native) = \"%s.cmxa\"
)\n" b.name b.desc (String.concat " " b.modules) b.name b.name
    end bindings;
    Buffer.contents buf

  (* Syntax extensions used internally *)
  let intern_syntaxes = ["trace"; "pa_obus"]
end

(***** Packages installed with ocamlfind *****)

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

(***** Utils *****)

let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "doc"; tag] f

let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

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

        (***** Dependencies checking *****)

        if not (Pathname.exists "_build/dependencies-checked") then
          execute ~quiet:true (Cmd(Sh"/bin/sh check-deps.sh > /dev/null"));

        (***** Library and bindings rules *****)

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

        (***** Various files auto-generation *****)

        rule "META" ~prod:"META"
          (fun _ _ -> Echo([Config.meta ()], "META"));

        rule "obus_doc" ~prod:"obus.odocl"
          (fun _ _ -> Echo(List.map (sprintf "obus/%s\n") Config.modules, "obus.odocl"));

        (***** Ocamlfind stuff *****)

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

        (***** Internal syntaxes *****)

        List.iter
          (fun tag ->
             flag_all_stages_except_link tag & S[A"-ppopt"; A("syntax/" ^ tag ^ ".cmo")];
             dep ["ocaml"; "ocamldep"; tag] ["syntax/" ^ tag ^ ".cmo"])
          Config.intern_syntaxes;

        (***** Other *****)

        (* Set the obus version number with pa_macro *)
        flag_all_stages_except_link "file:obus/oBus_info.ml" & S[A"-ppopt"; A(sprintf "-D OBUS_VERSION=%S" Config.obus_version)];

        (* Enable backtrace support if we have ocaml>=3.11 *)
        Scanf.sscanf Sys.ocaml_version "%d.%d" begin fun major minor ->
          if (major, minor) >= (3, 11) then
            flag_all_stages_except_link "pkg_camlp4.macro" & S[A"-ppopt"; A"-D HAVE_BACKTRACE"]
        end
    | _ -> ()
  end
