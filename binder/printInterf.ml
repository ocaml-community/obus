(*
 * printInterf.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types
open Introspect
open Printf
open Tree

let no_space_regexp = Str.regexp "[^ \t]"

let print real oc node =
  let rec aux indent (Node(interf, sons)) =
    let spaces = String.make indent ' ' in
    let p fmt = fprintf oc fmt in
    let pn fmt = ksprintf (fun s -> output_string oc spaces; output_string oc s) fmt in
    let n () = if real then output_string oc "\n" else () in

    let split_unindent str =
      let lines = StrUtil.split_lines str in
      let max_indent = List.fold_left begin fun n line ->
        try
          min (Str.search_forward no_space_regexp line 0) n
        with
            Not_found -> n
      end max_int lines in
        List.map (fun s -> Str.string_after s max_indent) lines
    in

    let print_doc indent docs =
      List.iter begin fun doc ->
        let spaces = String.make indent ' ' in
        let lines = split_unindent doc in
          match lines with
            | [] -> ()
            | first :: lines ->
                pn "%s(** %s" spaces first;
                List.iter (fun l -> n (); pn "%s    %s" spaces l) lines;
                p " *)\n"
      end docs in

    let print_func_type = function
      | [] -> p "unit -> "
      | l -> List.iter (fun t -> p "%s -> " (string_of_caml_type t)) l
    in

    let print_content name content proxy_typ =
      if real
      then begin
        n ();
        pn "(** {6 Wrapper for the dbus interface {i %s}} *)\n" name;
        n ();
        pn "type t\n";
        pn "val interface : t Interface.t\n"
      end;

      List.iter begin fun decl ->
        match decl with
          | Doc doc ->
              n ();
              print_doc 0 doc
          | Method(doc, dname, cname,
                   (in_args, ins),
                   (out_args, outs)) ->
              let ins = if real then proxy_typ :: ins else ins in
                n ();
                (* print sync version *)
                pn "val %s : " cname;
                print_func_type ins;
                p "%s\n" (string_of_caml_type (tuple outs));

                (* print method documentation *)
                print_doc 2 doc;

                if real
                then begin
                  (* print async version *)
                  pn "val %s_async : " cname;
                  print_func_type ins;
                  p "(";
                  print_func_type outs;
                  p "unit) -> unit\n";

                  (* print cookie version *)
                  pn "val %s_cookie : " cname;
                  print_func_type ins;
                  p "%s Cookie.t\n" (string_of_caml_type (tuple outs))
                end
          | Exception(doc, dname, cname) ->
              n ();
              pn "(** Error %S *)\n" dname;
              pn "exception %s of string\n" cname;
              print_doc 2 doc
          | Interf(interf, _) ->
              n ();
              List.iter (pn "%s\n") (split_unindent interf)
          | _ -> ()
      end content;

      (* Now print all signals *)
      let signals = Util.filter_map (function
                                       | Signal(doc, dname, cname, args) -> Some(doc, dname, cname, args)
                                       | _ -> None) content in

        if signals <> []
        then begin
          n ();
          pn "type signal =\n";
          List.iter begin fun (doc, dname, cname, (args, ctypes)) ->
            pn "  | %s of %s\n" cname (string_of_caml_type (tuple ctypes));
            print_doc 6 doc
          end signals;
          if real
          then begin
            n ();
            pn "val signals : (t, signal) Signal.set\n"
          end
        end in

      begin match interf with
        | None -> ()
        | Some(name, content, proxy_typ, to_proxy) -> print_content name content proxy_typ
      end;

      (* And finally sub-modules *)
      List.iter begin fun (name, node) ->
        n ();
        pn "module %s : sig\n" name;
        aux (indent + 2) node;
        pn "end\n"
      end sons
  in
    aux 0 node
