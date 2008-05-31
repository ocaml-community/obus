(*
 * introspect.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types
open Xparser

type flag_mode =
  | F_poly
  | F_variant
  | F_record
type proxy_type =
  | P_bus
  | P_connection
type name = string
type doc = string list
type argument = name option * name option * dbus_type * doc
type arguments = argument list * caml_type
type access = Read | Write | Read_write
type declaration =
  | Doc of doc
  | Method of doc * name * name * arguments * arguments
  | Signal of doc * name * name * arguments
  | Property of doc * name * name * dbus_type * access
  | Proxy of doc * name * proxy_type * string option * string option
  | Flag of doc * name * flag_mode *  (int * name * doc) list * bool * string option
  | Convert of doc * caml_type * caml_type * string option * string option
type module_tree = Node of name * declaration list * (name * module_tree) list

(* Camelization of names *)
let camlize_lid str = String.concat "_" (Util.split_upper str)

let mklid did = function
  | None -> camlize_lid did
  | Some cid -> cid

let mkuids did = function
  | None -> List.map String.capitalize (Util.split_dot did)
  | Some cid -> Util.split_dot cid

let rec default_caml_type = function
  | Tbyte -> char
  | Tboolean -> bool
  | Tint16 -> int
  | Tint32 -> int
  | Tint64 -> int64
  | Tuint16 -> int
  | Tuint32 -> int
  | Tuint64 -> int64
  | Tdouble -> float
  | Tstring -> string
  | Tsignature -> obus_dtypes
  | Tobject_path -> string
  | Tarray(t) -> list (default_caml_type t)
  | Tdict(tk, tv) -> list (tuple [default_caml_type tk; default_caml_type tv])
  | Tstructure(ts) -> tuple (List.map default_caml_type ts)
  | Tvariant -> obus_value

let mktyp dtyp = function
  | None -> default_caml_type dtyp
  | Some t -> t

let abf n = afd n false ["true", true; "false", false]
let abt n = afd n true ["true", true; "false", false]

(* Skip annotations *)
let annotations =
  (any (elt "annotation" (a2 (ars "name") (ars "value"))
          s0
          (fun _ _ -> ())))

let doc =
  any (elt "doc" a0 (s1 (one text)) (fun x -> x))

type direction = In | Out

let dir_in = [("in", In)]
let dir_out = [("out", Out)]
let dir_both = dir_in @ dir_out

let adname = ars "name"
let acname = aos "cname"
let adtype = ar "type" dbus_type_of_signature
let actype = ao "ctype" caml_type_of_string

let arguments dirs =
  any (elt "arg" (a5 (aos "name") (aos "cname") (afd "direction" In dirs) adtype actype)
         (s1 doc)
         (fun dname cname dir dtyp ctyp doc ->
            (dir, (dname, cname, dtyp, doc), mktyp dtyp ctyp)))

let remove_dir = List.map (fun (a, b, c) -> (b, c))

let method_decl =
  elt "method" (a3 adname acname actype)
    (s2 (arguments dir_both) doc)
    (fun dname cname ctyp args doc ->
       let ins, outs = Util.part_map
         (function
            | (In, a, b) -> Some(a, b)
            | (Out, _, _) -> None) args in
       let outs = remove_dir outs in
       let in_args, in_ctyps = List.split ins
       and out_args, out_ctyps = List.split outs in
       let caml_in_typ, caml_out_typ = match ctyp with
         | Some t -> begin match List.rev (list_of_tuple t) with
             | t :: ts -> (tuple (List.rev ts), t)
             | _ -> failwith
                 (Printf.sprintf
                    "invalid caml type for a method: %s, must have the form 'a -> 'b"
                    (string_of_caml_type t))
           end
         | None -> (tuple in_ctyps, tuple out_ctyps)
       in
         Method(doc, dname, mklid dname cname, (in_args, caml_in_typ), (out_args, caml_out_typ)))

let signal_decl =
  elt "signal" (a3 adname acname actype)
    (s2 (arguments dir_in) doc)
    (fun dname cname ctyp args doc ->
       let args, ctyps = List.split (remove_dir args) in
         Signal(doc, dname, String.capitalize (mklid dname cname),
                (args,
                 match ctyp with
                   | Some t -> t
                   | None -> tuple ctyps)))

let proxy_decl =
  elt "proxy" (a4 adname (afd "type" P_bus ["bus", P_bus; "connection", P_connection])
                 (aos "destination") (aos "path"))
    (s1 doc)
    (fun name typ dest path doc ->
       Proxy(doc, name, typ, dest, path))

let flag_decl =
  elt "flag" (a4 (ars "name") (afd "mode" F_poly ["poly", F_poly; "variant", F_variant; "record", F_record])
                (abf "bitwise") (aos "module"))
    (s2
       (any (elt "value" (a2 (ar "key" int_of_string) (ars "name"))
               (s1 doc)
               (fun key name doc -> (key, name, doc))))
       doc)
    (fun name mode bitwise modul values doc ->
       Flag(doc, name, mode,
            (if mode = F_record
             then values
             else List.map (fun (k, n, d) -> (k, String.capitalize n, d)) values),
            bitwise, modul))

let convert_decl =
  elt "convert" (a4
                   (ar "a" caml_type_of_string)
                   (ar "b" caml_type_of_string)
                   (aos "a_of_b")
                   (aos "b_of_a"))
    (s1 doc)
    (fun a b a_of_b b_of_a doc ->
       Convert(doc, a, b, a_of_b, b_of_a))

let doc_decl =
  elt "doc" a0 (s1 (any text)) (fun x -> Doc x)

let interface =
  elt "interface" (a2 adname acname)
    (s1 (union [method_decl;
                signal_decl;
                proxy_decl;
                flag_decl;
                convert_decl;
                doc_decl]))
    (fun dname cname decls ->
       (dname, mkuids dname cname, decls))

let node =
  elt "node" a0
    (s2
       (any interface)
       (any (elt "node" a0
               s0
               ())))
    (fun interfs _ -> interfs)

let rec insert node x = match node, x with
  | Node(_, _, sons), (dname, [], content) ->
      Node(dname, content, sons)
  | Node(dname, content, sons), (dname', cname :: cnames, content') ->
      Node(dname, content, insert_in_sons cname (dname', cnames, content') sons)
and insert_in_sons cname x = function
  | [] -> [(cname, insert (Node("", [], [])) x)]
  | (cname', node) :: sons when cname' = cname -> (cname, insert node x) :: sons
  | (cname', node) :: sons -> (cname', node) :: insert_in_sons cname x sons

let parse_files fnames =
  let interfs = List.flatten (List.map (fun fn ->
                                          let xml = Util.parse_xml fn in
                                            parse node ~filename:fn xml) fnames) in
    List.fold_left insert (Node("", [], [])) interfs

let contain_dbus_declaration = List.exists (function
                                              | Method _
                                              | Signal _
                                              | Property _ -> true
                                              | _ -> false)
