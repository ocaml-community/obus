(*
 * genInterf.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Types
open Introspect

let _loc = Loc.ghost

let print_for_doc oc interf =
  let newline () = output_string oc "\n" in
  let rec aux indent (Node(_, content, sons)) =
    let print fmt = print_string indent; Printf.fprintf oc (fmt ^^ "\n") in
      begin match List.exists (function
                                 | Method _
                                 | Signal _
                                 | Property _ -> true
                                 | _ -> false) content with
        | false -> ()
        | true ->
            print "type t";
            print "type proxy = t OBus.Proxy.t";
            newline ();
            print "val interface : t OBus.Interface.t"
      end;
      List.iter begin fun decl ->
        newline ();
        match decl with
          | Method(_, cname, (_, in_caml_type), (_, out_caml_type)) ->
              let ctypes = list_of_tuple in_caml_type @ [out_caml_type] in
                print "val %s : proxy -> %s" cname (String.concat " -> " (List.map (string_of_type "unit") ctypes));
                print "  (** [%s proxy] *)" cname
          | _ -> ()
      end content;
      List.iter begin fun (name, node) ->
        newline ();
        print "module %s : sig" name;
        aux ("  " ^ indent) node;
        print "end"
      end sons
  in
    aux "" interf

let rec gen (Node(_, content, sons)) =
  let sg = Ast.sgSem_of_list begin List.map begin function
    | Method(dname, cname, (in_args, in_caml_type), (out_args, out_caml_type)) ->
        let ctypes = typ "proxy" [] :: list_of_tuple in_caml_type in
          (<:sig_item<
           val $lid:cname$ : $ctyp_func_of_types ctypes (ctyp_of_type out_caml_type)$
           val $lid:cname ^ "_async"$ : $ctyp_func_of_types ctypes
             (<:ctyp<
                $ctyp_func_of_types (match list_of_tuple out_caml_type with [] -> [unit] | l -> l) <:ctyp< unit >>$
                -> unit >>)$
           val $lid:cname ^ "_cookie"$ : $ctyp_func_of_types ctypes
             (ctyp_of_type (typ "Cookie.t" [out_caml_type]))$
             >>)
    | _ -> (<:sig_item< >>)
  end content end in
  let sg = match List.exists (function
                                | Method _
                                | Signal _
                                | Property _ -> true
                                | _ -> false) content with
    | false -> sg
    | true -> (<:sig_item<
               type t
               type proxy = t Proxy.t
               val interface : t Interface.t;;
               $sg$
               >>) in
    Ast.sgSem_of_list
      (sg :: List.map (fun (name, node) ->
                         (<:sig_item<
                          module $uid:name$ : sig
                            $gen node$
                          end >>)) sons)
