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

let no_space_regexp = Str.regexp "[^ \t]"

let print internal oc node =
  let proxy_typ = match internal with
    | true -> "Bus.t"
    | false -> "[> t ] Proxy.t"
  in

  let rec aux indent (Node(name, content, sons)) =
    let spaces = String.make indent ' ' in
    let p fmt = fprintf oc fmt in
    let pn fmt = ksprintf (fun s -> output_string oc spaces; output_string oc s) fmt in
    let n () = output_string oc "\n" in

    let print_doc indent docs =
      List.iter begin fun doc ->
        let spaces = String.make indent ' ' in
        let lines = Util.split_lines doc in
        let max_indent = List.fold_left begin fun n line ->
          try
            max (Str.search_forward no_space_regexp line 0) n
          with
              Not_found -> n
        end 0  lines in
        let lines = List.map (fun s -> Str.string_after s max_indent) lines in
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

    let print_flag doc prefix values =
      List.iter begin fun (key, name, doc) ->
        pn "  | %s%s\n" prefix name;
        print_doc 6 doc
      end values in

      begin match contain_dbus_declaration content with
        | false -> ()
        | true ->
            n ();
            pn "(** {6 Wrapper for the dbus interface {i %s}} *)\n" name;
            n ();
            pn "type t = [ `%s ]\n" (String.capitalize (Str.global_replace Util.dot_regexp "_" name));
            pn "val interface : t Interface.t\n"
      end;
      List.iter begin fun decl ->
        match decl with
          | Doc doc ->
              n ();
              print_doc 0 doc
          | Method(doc, dname, cname,
                   (in_args, in_caml_type),
                   (out_args, out_caml_type)) ->
              let ins = typ proxy_typ [] :: list_of_tuple in_caml_type
              and outs = list_of_tuple out_caml_type in
                n ();
                (* print sync version *)
                pn "val %s : " cname;
                print_func_type ins;
                p "%s\n" (string_of_caml_type out_caml_type);

                (* print method documentation *)
                if doc = []
                then pn "  (** Wrapper for %s *)\n" dname
                else print_doc 2 doc;

                (* print async version *)
                pn "val %s_async : " cname;
                print_func_type ins;
                p "(";
                print_func_type outs;
                p "unit) -> unit\n";

                (* print cookie version *)
                pn "val %s_cookie : " cname;
                print_func_type ins;
                p "%s Cookie.t\n" (string_of_caml_type out_caml_type)
          | Proxy(doc, name, ptyp, dest, path) ->
              let args = Util.filter_map (fun x -> x)
                [ (match ptyp with
                     | P_bus -> Some (typ "Bus.t" [])
                     | P_connection -> Some (typ "Connection.t" []));
                  (match dest with
                     | Some _ -> None
                     | None -> Some (typ "Proxy.name" []));
                  (match path with
                     | Some _ -> None
                     | None -> Some (typ "Proxy.path" [])) ] in
                n ();
                pn "val %s : " name;
                print_func_type args;
                p "t Proxy.t\n";
                print_doc 2 doc;
          | Flag(doc, name, _, [], _, None) ->
              n ();
              pn "type %s\n" name;
              print_doc 2 doc
          | Flag(doc, name, mode, values, bitwise, None) ->
              n ();
              begin match mode with
                | F_poly ->
                    pn "type %s =\n" name;
                    print_doc 2 doc;
                    pn "  [\n";
                    print_flag doc "`" values;
                    pn "  ]\n"
                | F_variant ->
                    pn "type %s =\n" name;
                    print_doc 2 doc;
                    print_flag doc "" values
                | F_record ->
                    print_doc 0 doc;
                    pn "type %s = {\n" name;
                    List.iter begin fun (key, name, doc) ->
                      p "  %s : bool\n" name;
                    end values;
                    pn "}\n";
              end
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
          List.iter begin fun (doc, dname, cname, (args, ctype)) ->
            pn "  | %s of %s\n" cname (string_of_caml_type ctype);
            if doc = []
            then pn "      (** Representation of signal %s *)\n" dname
            else print_doc 6 doc
          end signals
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
