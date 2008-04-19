(*
 * binder.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

module MakeMap(L : Language.S) =
struct
  open L

  let xml_fnames = ref []
  let skipped_interfaces = ref []
  let permit_default = ref false
  let out = ref "out.obus.xml"
  let flat = ref false

  let args = [
    ("-skip", Arg.String (fun s -> skipped_interfaces := s :: !skipped_interfaces), "skip interfaces matching regexp");
    ("-allow-default", Arg.Set permit_default, "do not skip default interfaces");
    ("-o", Arg.Set_string out, Printf.sprintf "output file (default = %s)" !out);
    ("-flat", Arg.Set flat, "do not include interfaces in each other")
  ] @ L.args_mapper

  let make_args dargs =
    List.map (fun (Sig.Arg(dname, dtype)) -> Sig.Arg(L.correct_arg_name dname,
                                                     L.default_type dtype)) dargs

  let make_defs ddefs =
    List.map begin function
      | Sig.Method(dname, dins, douts) ->
          Sig.Method(L.correct_method_name dname,
                     make_args dins,
                     make_args douts)
      | Sig.Signal(dname, dargs) ->
          Sig.Signal(L.correct_method_name dname,
                     make_args dargs)
    end ddefs

  let pt = Str.regexp "\\."

  let make_tree interfaces =
    let map, sigs = List.split begin List.map begin fun (Sig.Sig(dname, ddefs) as interf) ->
      let lname = Util.ljoin "." (List.map L.correct_module_name (Str.split pt dname)) in
        ((lname, interf),
         (lname, make_defs ddefs))
    end interfaces end in
      (map, List.fold_left (fun acc (lname, ldefs) -> Sig.add lname ldefs acc) Sig.empty sigs)

  let make_flat interfaces =
    let map, sigs = List.split begin List.map begin fun (Sig.Sig(dname, ddefs) as interf) ->
      let lname = L.correct_module_name (Str.global_replace pt "" dname) in
        ((lname, interf),
         (Sig.Sig(lname, make_defs ddefs), Sig.Tree []))
    end interfaces end in
      (map, Sig.Tree sigs)

  let main () =
    Arg.parse args
      (fun s -> xml_fnames := s :: !xml_fnames)
      "generate a mapping file from introspection data";
    if not !permit_default
    then skipped_interfaces := !skipped_interfaces @ ["org\\.freedesktop\\.DBus.*"];

    let filters = List.map Str.regexp !skipped_interfaces in
    let accept name = not (List.exists (fun r -> Str.string_match r name 0) filters) in
    let dbus_interfaces = List.filter
      (fun (Sig.Sig(name, defs)) -> accept name)
      (List.flatten (List.map (fun fname -> DBus.from_xml (Util.parse_xml fname)) !xml_fnames)) in
    let map, sigs = (if !flat then make_flat else make_tree) dbus_interfaces in
    let mapping = { Lmap.language = L.name;
                    Lmap.sigs = sigs;
                    Lmap.map = map }
    in
      Util.with_open_out !out begin fun ch ->
        Printf.fprintf ch "<!-- File generated with %s -->\n" (Filename.basename (Sys.argv.(0)));
        output_string ch (Xml.to_string_fmt (Lmap.to_xml string_of_type mapping))
      end
end

module MakeGen(L : Language.S) =
struct
  let out = ref "out"
  let map_files = ref []

  let args = [
    ("-o", Arg.Set_string out, Printf.sprintf "output file prefix (default = %s)" !out)
  ] @ L.args_generator

  let main () =
    Arg.parse args
      (fun s -> map_files := s :: !map_files)
      "generate a caml module from a mapping";

    let mapping =
      List.fold_left Lmap.merge (Lmap.empty L.name)
        (List.map (fun s -> Lmap.from_xml L.type_of_string (Util.parse_xml s)) !map_files) in
      L.generate !out mapping
end
