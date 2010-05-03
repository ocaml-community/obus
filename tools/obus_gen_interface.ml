(*
 * obus_gen_interface.ml
 * ---------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open OBus_introspect

let prog_name = Filename.basename Sys.argv.(0)

let print_record oc members =
  output_string oc "  type 'a members= {\n";
  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           fprintf oc "    m_%s : %a -> 'a -> %a -> %a;\n"
             name
             (Term.print_intf true)
             (Term.T.term "OBus_context.t"
                [Term.intf_of_sequence (List.map snd o_args)])
             (Term.print_intf true)
             (Term.intf_of_sequence (List.map snd i_args))
             (Term.print_intf true)
             (Term.T.term "Lwt.t" [Term.T.term "unit" []])
       | Signal(name, args, annotations) ->
           ()
       | Property(name, typ, Read, annotations) ->
           fprintf oc "    p_%s : 'a -> %a;\n"
             name
             (Term.print_intf true)
             (Term.T.term "React.signal" [Term.intf_of_single typ])
       | Property(name, typ, Write, annotations) ->
           fprintf oc "    p_%s : unit OBus_context.t -> 'a -> %a -> unit Lwt.t;\n"
             name
             (Term.print_intf true)
             (Term.intf_of_single typ)
       | Property(name, typ, Read_write, annotations) ->
           fprintf oc "    p_%s : ('a -> %a) * (unit OBus_context.t -> 'a -> %a -> unit Lwt.t);\n"
             name
             (Term.print_intf true)
             (Term.T.term "React.signal" [Term.intf_of_single typ])
             (Term.print_intf true)
             (Term.intf_of_single typ))
    members;
  output_string oc "  }\n"

(* +-----------------------------------------------------------------+
   | Implementation generation                                       |
   +-----------------------------------------------------------------+ *)

let print_args oc args =
  fprintf oc "(arg%d" (List.length args);
  List.iter
    (function
       | (None, typ) ->
           fprintf oc "\n                       (None, %a)"
             (Term.print_impl true) (Term.impl_of_single typ)
       | (Some name, typ) ->
           fprintf oc "\n                       (Some %S, %a)"
             name (Term.print_impl true) (Term.impl_of_single typ))
    args;
  output_char oc ')'

let print_impl oc name members annotations =
  fprintf oc "module %s =\n\
              struct\n\
             \  let interface = %S\n"
    (String.capitalize (Utils.file_name_of_interface_name name))
    name;

  (***** Member description *****)

  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           fprintf oc "  let m_%s = {\n\
                      \    Method.interface = interface;\n\
                      \    Method.member = %S;\n\
                      \    Method.i_args = %a;\n\
                      \    Method.o_args = %a;\n\
                      \  }\n"
             name name print_args i_args print_args o_args
       | Signal(name, args, annotations) ->
           fprintf oc "  let s_%s = {\n\
                      \    Signal.interface = interface;\n\
                      \    Signal.member = %S;\n\
                      \    Signal.args = %a;\n\
                      \  }\n"
             name name print_args args
       | Property(name, typ, access, annotations) ->
           fprintf oc "  let p_%s = {\n\
                      \    Property.interface = interface;\n\
                      \    Property.member = %S;\n\
                      \    Property.typ = %a;\n\
                      \    Property.access = Property.%s;\n\
                      \  }\n"
             name name (Term.print_impl true) (Term.impl_of_single typ)
             (match access with
                | Read -> "readable"
                | Write -> "writable"
                | Read_write -> "readable_writable"))
    members;

  (***** Interface description *****)

  print_record oc members;
  output_string oc "  let make ?notify_mode members =\n";
  output_string oc "    OBus_object.make_interface_unsafe ?notify_mode interface\n\
                   \      [|\n";
  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           fprintf oc "        method_info m_%s members.m_%s;\n" name name
       | _ ->
           ())
    members;
  output_string oc "      |]\n      [|\n";
  List.iter
    (function
       | Signal(name, args, annotations) ->
           fprintf oc "        signal_info s_%s;\n" name
       | _ ->
           ())
    members;
  output_string oc "      |]\n      [|\n";
  List.iter
    (function
       | Property(name, typ, Read, annotations) ->
           fprintf oc "        property_r_info p_%s members.p_%s;\n" name name
       | Property(name, typ, Write, annotations) ->
           fprintf oc "        property_w_info p_%s members.p_%s;\n" name name
       | Property(name, typ, Read_write, annotations) ->
           fprintf oc "        property_rw_info p_%s (fst members.p_%s) (snd members.p_%s);\n" name name name
       | _ ->
           ())
    members;
  output_string oc "      |]\n";
  output_string oc "end\n"

(* +-----------------------------------------------------------------+
   | Interface generation                                            |
   +-----------------------------------------------------------------+ *)

let print_intf oc name members annotations =
  fprintf oc "module %s : sig\n" (String.capitalize (Utils.file_name_of_interface_name name));
  fprintf oc "  val interface : OBus_name.interface\n";

  (***** Member description *****)

  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           fprintf oc "  val m_%s : %a\n"
             name
             (Term.print_intf true)
             (Term.T.term "Method.t"
                [Term.intf_of_sequence (List.map snd i_args);
                 Term.intf_of_sequence (List.map snd o_args)])
       | Signal(name, args, annotations) ->
           fprintf oc "  val s_%s : %a\n"
             name
             (Term.print_intf true)
             (Term.T.term "Signal.t"
                [Term.intf_of_sequence (List.map snd args)])
       | Property(name, typ, access, annotations) ->
           fprintf oc "  val p_%s : %a\n"
             name
             (Term.print_intf true)
             (Term.T.term "Property.t"
                [Term.intf_of_single typ;
                 Term.T.term
                   (match access with
                      | Read -> "[ `readable ]"
                      | Write -> "[ `writable ]"
                      | Read_write -> "[ `readable | `writable ]")
                   []]))
    members;

  (***** Interface description *****)

  print_record oc members;
  output_string oc "  val make : ?notify_mode : 'a OBus_object.notify_mode -> 'a members -> 'a OBus_object.interface\n";
  output_string oc "end\n"

(* +-----------------------------------------------------------------+
   | Sorting                                                         |
   +-----------------------------------------------------------------+ *)

let compare_members ma mb =
  match ma, mb with
    | Method(name_a, i_args_a, _, _), Method(name_b, i_args_b, _, _) -> begin
        match String.compare name_a name_b with
          | 0 ->
              Pervasives.compare (List.map snd i_args_a) (List.map snd i_args_b)
          | n ->
              n
      end
    | Signal(name_a, _, _), Signal(name_b, _, _) ->
        String.compare name_a name_b
    | Property(name_a, _, _, _), Property(name_b, _, _, _) ->
        String.compare name_a name_b
    | Method _, _ -> -1
    | _, Method _ -> 1
    | Signal _, _ -> -1
    | _, Signal _ -> 1

let sort_members members = List.sort compare_members members

(* +-----------------------------------------------------------------+
   | Entry-point                                                     |
   +-----------------------------------------------------------------+ *)

let usage_message =
  Printf.sprintf "Usage: %s <options> <files>\n\
                  Generate OCaml modules for D-Bus interfaces.\n\
                  options are:" prog_name

let keep_common = ref false
let prefix = ref "obus_interface"

let args = [
  "-keep-common", Arg.Set keep_common, "do not ignore common interfaces";
  "-o", Arg.Set_string prefix, "<prefix> output file prefix";
 ]

let () =
  let sources = ref [] in
  Arg.parse args (fun s -> sources := s :: !sources) usage_message;

  if !sources = [] then begin
    Arg.usage args usage_message;
    exit 1
  end;

  (* Parse source files *)
  let interfaces =
    List.fold_left
      (fun acc file_name ->
         Utils.IFSet.union acc (Utils.parse_file file_name))
      Utils.IFSet.empty
      !sources
  in

  let oc_impl = open_out (!prefix ^ ".ml") and oc_intf = open_out (!prefix ^ ".mli") in

  fprintf oc_impl
    "(* File auto-generated by %s, DO NOT EDIT. *)\n\
     open OBus_value\n\
     open OBus_value.C\n\
     open OBus_member\n\
     open OBus_object\n"
    prog_name;

  fprintf oc_intf
    "(* File auto-generated by %s, DO NOT EDIT. *)\n\
     open OBus_member\n"
    prog_name;

  Utils.IFSet.iter
    (fun (name, members, annotations) ->
       if !keep_common ||
         (match OBus_name.split name with
            | "org" :: "freedesktop" :: "DBus" :: _ -> false
            | _ -> true) then begin
           let members = sort_members members in
           print_impl oc_impl name members annotations;
           print_intf oc_intf name members annotations
         end)
    interfaces;

  close_out oc_impl;
  close_out oc_intf;

  printf "file \"%s.ml\" written\n" !prefix;
  printf "file \"%s.mli\" written\n" !prefix
