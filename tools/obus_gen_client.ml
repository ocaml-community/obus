(*
 * obus_gen_client.ml
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open OBus_introspect
open OBus_value

let prog_name = Filename.basename Sys.argv.(0)

let make_names args =
  let _, l =
    List.fold_left
      (fun (n, l) (name, typ) ->
         match name with
           | None -> (n + 1, (false, "x" ^ string_of_int n) :: l)
           | Some "type" -> (n, (true, "typ") :: l)
           | Some name -> (n, (true, name) :: l))
      (1, [])
      args
  in
  List.rev l

(* +-----------------------------------------------------------------+
   | Implementation generation                                       |
   +-----------------------------------------------------------------+ *)

let print_names oc = function
  | [] ->
      output_string oc "()";
  | [(_, name)] ->
      output_string oc name
  | (_, name) :: names ->
      output_char oc '(';
      output_string oc name;
      List.iter (fun (_, name) -> fprintf oc ", %s" name) names;
      output_char oc ')'

let rec contains_path = function
  | T.Basic T.Object_path -> true
  | T.Basic _ -> false
  | T.Array t -> contains_path t
  | T.Dict(T.Object_path, _) -> true
  | T.Dict(_, tv) -> contains_path tv
  | T.Structure tl -> List.exists contains_path tl
  | T.Variant -> false

let make_convertors make_convertor names args =
  List.map2
    (fun (_, name) (_, typ) -> match make_convertor typ with
       | Some f -> Some(sprintf "let %s = %s %s in\n" name f name)
       | None -> None)
    names args

let print_impl oc name members annotations =
  let module_name = String.capitalize (Utils.file_name_of_interface_name name) in
  fprintf oc "\n\
              module %s =\n\
              struct\n\
             \  open %s\n\
              \n\
             \  let notify_mode = OBus_property.notify_none\n"
    module_name module_name;
  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           let i_names = make_names i_args and o_names = make_names o_args in
           let i_convertors = make_convertors Utils.convertor_send i_names i_args
           and o_convertors = make_convertors Utils.convertor_recv o_names o_args in
           fprintf oc "\n  let %s proxy" (OBus_name.ocaml_lid name);
           List.iter
             (function
                | (false, name) -> fprintf oc " %s" name
                | (true, name) -> fprintf oc " ~%s" name)
             i_names;
           output_string oc " =\n";
           List.iter
             (function
                | Some line -> fprintf oc "    %s" line
                | None -> ())
             i_convertors;
           let need_context = List.exists (fun (_, typ) -> contains_path typ) o_args in
           if List.for_all (fun conv -> conv = None) o_convertors then begin
             fprintf oc "    OBus_method.call m_%s proxy " name;
             print_names oc i_names;
             output_char oc '\n'
           end else begin
             output_string oc "    lwt ";
             if need_context then output_string oc "(context, ";
             print_names oc o_names;
             if need_context then
               fprintf oc ") = OBus_method.call_with_context m_%s proxy " name
             else
               fprintf oc " = OBus_method.call m_%s proxy " name;
             print_names oc i_names;
             output_string oc " in\n";
             List.iter
               (function
                  | Some line -> fprintf oc "    %s" line
                  | None -> ())
               o_convertors;
             output_string oc "    return ";
             print_names oc o_names;
             output_char oc '\n'
           end
       | Signal(name, args, annotations) ->
           let names = make_names args in
           let convertors = make_convertors Utils.convertor_recv names args in
           fprintf oc "\n  let %s proxy =\n" (OBus_name.ocaml_lid name);
           if List.for_all (fun x -> x = None) convertors then
             fprintf oc "    OBus_signal.connect s_%s proxy\n" name
           else begin
             if List.exists (fun (_, typ) -> contains_path typ) args then
               output_string oc "    OBus_signal.map_with_context\n\
                                \      (fun context "
             else
               output_string oc "    OBus_signal.map\n\
                                \      (fun ";
             print_names oc names;
             output_string oc " ->\n";
             List.iter
               (function
                  | Some line -> fprintf oc "         %s" line
                  | None -> ())
               convertors;
             output_string oc "         ";
             print_names oc names;
             output_string oc ")\n";
             fprintf oc "      (OBus_signal.connect s_%s proxy)\n" name
           end
       | Property(name, typ, access, annotations) ->
           fprintf oc "\n  let %s proxy =\n" (OBus_name.ocaml_lid name);
           match Utils.convertor_recv typ, Utils.convertor_send typ with
             | Some f_recv, Some f_send -> begin
                 let need_context = contains_path typ in
                 fprintf oc "    OBus_property.map_%s%s\n"
                   (match access with
                      | Read -> "r"
                      | Write -> "w"
                      | Read_write -> "rw")
                   (if need_context then "_with_context" else "");
                 let ctx = if need_context then " context" else "" in
                 if access = Read || access = Read_write then
                   fprintf oc "      (fun%s x -> %s x)\n" ctx f_recv;
                 if access = Write || access = Read_write then
                   fprintf oc "      (fun x -> %s x)\n" f_send;
                 fprintf oc "      (OBus_property.make p_%s ~notify_mode proxy)\n" name
               end
             | None, None ->
                 fprintf oc "    OBus_property.make p_%s ~notify_mode proxy\n" name
             | _ ->
                 assert false)
    members;
  output_string oc "end\n"

(* +-----------------------------------------------------------------+
   | Interface generation                                            |
   +-----------------------------------------------------------------+ *)

let print_intf oc name members annotations =
  fprintf oc "\nmodule %s : sig\n" (String.capitalize (Utils.file_name_of_interface_name name));
  List.iter
    (function
       | Method(name, i_args, o_args, annotations) ->
           fprintf oc "  val %s : OBus_proxy.t -> " (OBus_name.ocaml_lid name);
           List.iter
             (function
                | (None, typ) ->
                    fprintf oc "%a -> " (Term.print_intf true) (Term.client_of_single typ)
                | (Some name, typ) ->
                    fprintf oc "%s : %a -> " name (Term.print_intf true) (Term.client_of_single typ))
             i_args;
           Term.print_intf true oc
             (Term.T.term "Lwt.t"
                [Term.T.tuple
                   (List.map (fun (_, typ) -> Term.client_of_single typ) o_args)]);
           output_char oc '\n'
       | Signal(name, args, annotations) ->
           fprintf oc "  val %s : OBus_proxy.t -> %a\n"
             (OBus_name.ocaml_lid name)
             (Term.print_intf true)
             (Term.T.term "OBus_signal.t"
                [Term.T.tuple (List.map (fun (_, typ) -> Term.client_of_single typ) args)])
       | Property(name, typ, access, annotations) ->
           fprintf oc "  val %s : OBus_proxy.t -> %a\n"
             (OBus_name.ocaml_lid name)
             (Term.print_intf true)
             (Term.T.term "OBus_property.t"
                [Term.client_of_single typ;
                 Term.T.term
                   (match access with
                      | Read -> "[ `readable ]"
                      | Write -> "[ `writable ]"
                      | Read_write -> "[ `readable | `writable ]")
                   []]))
    members;
  output_string oc "end\n"

(* +-----------------------------------------------------------------+
   | Entry-point                                                     |
   +-----------------------------------------------------------------+ *)

let usage_message =
  Printf.sprintf "Usage: %s <options> <files>\n\
                  Generate OCaml proxy code for D-Bus interfaces.\n\
                  options are:"
    prog_name

let keep_common = ref false
let prefix = ref "obus_client"

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

  output_string oc_impl "open Lwt\n";

  Utils.IFSet.iter
    (fun (name, members, annotations) ->
       if !keep_common ||
         (match OBus_name.split name with
            | "org" :: "freedesktop" :: "DBus" :: _ -> false
            | _ -> true) then begin
           print_impl oc_impl name members annotations;
           print_intf oc_intf name members annotations
         end)
    interfaces;

  close_out oc_impl;
  close_out oc_intf;

  printf "file \"%s.ml\" written\n" !prefix;
  printf "file \"%s.mli\" written\n" !prefix
