open Ocamlbuild_plugin
open Command (* no longer needed for OCaml >= 3.10.2 *)

let append dir fnames = List.map (fun fname -> dir / fname) fnames

module Config =
struct
  (* Files of binder which use camlp4 quotation for caml ast *)
  let code_generators =
    ["genCode";
     "genSerializer";
     "env";
     "gen-header-rw";
     "codeConstants";
     "helpers"]

  (* Files using each syntax *)
  let use_syntax =
    ["seq", ["binder/genSerializer.ml"];
     "log", append "obus" ["auth.ml"; "transport.ml"; "connection.ml"]];
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

let myexts () =
  List.map (fun x -> Pathname.basename (Pathname.remove_extension x))
    (blank_sep_strings &
       Lexing.from_string &
       run_and_read "echo syntax/*.ml")

let _ =
  List.iter
    (fun module_name ->
       tag_file (Printf.sprintf "binder/%s.ml" module_name) ["use_camlp4"; "camlp4of"; "pa_gen_code"])
    Config.code_generators;

  List.iter
    (fun (syntax, fnames) ->
       List.iter
         (fun fname ->
            let tags = tags_of_pathname fname in
              if not (Tags.mem "camlp4o" tags or Tags.mem "camlp4of" tags)
              then tag_file fname ["camlp4o"];
              tag_file fname ["pa_" ^ syntax])
         fnames)
    Config.use_syntax;

  dispatch begin function
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        Options.ocamlc   := ocamlfind & A"ocamlc";
        Options.ocamlopt := ocamlfind & A"ocamlopt";
        Options.ocamldep := ocamlfind & A"ocamldep";
        Options.ocamldoc := ocamlfind & A"ocamldoc"

    | After_rules ->

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

        List.iter begin fun ext ->
          flag ["ocaml"; "pp"; ext] & A("syntax/"^ext^".cmo");
          dep ["ocaml"; "ocamldep"; ext] ["syntax/"^ext^".cmo"];
        end (myexts ());

        (* For compiling the library without knowing the
           implementation of thread modules *)
        flag ["ocaml"; "compile"; "threadsigs"] & S[A"-I"; A"obus/threadsigs"];

        (* For samples to find .cmi files *)
        flag ["ocaml"; "compile"; "samples"] & S[A"-I"; A"obus"];
        flag ["ocaml"; "link"; "samples"] (A"obus.cma");
        dep ["ocaml"; "samples"] ["obus.cma"];

        flag ["ocaml"; "compile"; "samples-binding"] & S[A"-I"; A"binder"];
        flag ["ocaml"; "link"; "samples-binding"] (A"obus-binding.cma");
        dep ["ocaml"; "samples-binding"] ["obus-binding.cma"];

        (* For testing programs, let them access to all modules *)
        flag ["ocaml"; "compile"; "test"] & S[A"-I"; A"obus"; A"-I"; A"binder"];
        flag ["ocaml"; "link"; "test"] & S[A"-I"; A"obus"; A"-I"; A"binder"];

        (* Rule for automatically generating some files of the obus
           library using the binder tool *)
        rule "obus-header-serializer"
          ~prod:"obus/headerRW.ml"
          ~dep:"binder/gen-header-rw.byte"
          (fun env builder ->
             (Cmd (Sh "./binder/gen-header-rw.byte > obus/headerRW.ml")))

    | _ -> ()
  end
