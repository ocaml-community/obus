(*
 * binder.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

module type LanguageType =
sig
  type mono
  type poly
    (** Represention of the language type *)

  type expr
  type module_sig = mono Interface.t
  type module_str

  val parse_module_sigs : string -> module_sig Tree.t

  val default_type : DBus.typ -> mono
    (** [default typ] The default type for a dbus type *)

  val write_module_sigs : string -> module_sig Tree.t -> unit
  val write_module_strs : string -> module_str Tree.t -> unit

  val args : (Arg.key * Arg.spec * Arg.doc) list
end

module MakeSig(L : LanguageType) =
struct
  open L

  let xml_fnames = ref []
  let skipped_interfaces = ref []
  let permit_default = ref false
  let prefix = ref ["org\\.freedesktop\\."]
  let out = ref "out.mli"

  let args = [
    ("-skip", Arg.String (fun s -> skipped_interfaces := s :: !skipped_interfaces), "skip interfaces matching regexp");
    ("-allow-default", Arg.Set permit_default, "do not skip default interfaces");
    ("-o", Arg.Set_string out, "output file (default = out.mli)");
    ("-prefix", Arg.String (fun p -> prefix := p :: !prefix), "prefix to delete from interface names")
  ] @ L.args

  let main () =
    Arg.parse args
      (fun s -> xml_fnames := s :: !xml_fnames)
      "generate a caml module signature from a introspection data";
    if not !permit_default
    then skipped_interfaces := !skipped_interfaces @ ["org\\.freedesktop\\.DBus.*"];

    let filters = List.map Str.regexp !skipped_interfaces in
    let prefix = List.map Str.regexp !prefix in
    let accept name = not (List.exists (fun r -> Str.string_match r name 0) filters) in
    let rec delete_prefix name = function
      | [] -> name
      | prefix :: prefixes ->
          if Str.string_match prefix name 0
          then Str.replace_first prefix "" name
          else delete_prefix name prefixes
    in
    let dbus_interfaces = List.fold_left
      (fun acc (name, interf) -> if accept name then Tree.add (delete_prefix name prefix) interf acc else acc)
      Tree.empty
      (List.flatten (List.map (fun fname -> Util.with_open_in fname
                                 (fun ch -> Interface.from_instrospection
                                    (Lexing.from_channel ch))) !xml_fnames)) in
    let lang_interfaces = Tree.map (Interface.map default_type) dbus_interfaces in
      write_module_sigs !out lang_interfaces

end

module MakeStr(L : LanguageType) =
struct
  let main () = ()
end
