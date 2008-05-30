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

type name = string
type argument = name option * name option * dbus_type
type arguments = argument list * caml_type
type access = Read | Write | Read_write
type declaration =
  | Method of name * name * arguments * arguments
  | Signal of name * name * arguments
  | Property of name * name * dbus_type * access
  | Proxy of name * [ `Bus | `Connection ] * string option * string option
  | Flag of name * (int * name) list
type module_tree = Node of name * declaration list * (name * module_tree) list

(* Camelization of names *)
let camlize_lid str = String.concat "_" (Util.split_upper str)

let mklid did = function
  | None -> camlize_lid did
  | Some cid -> cid

let dot_regexp = Str.regexp "\\."

let mkuids did = function
  | None -> List.map String.capitalize (Str.split dot_regexp did)
  | Some cid -> Str.split dot_regexp cid

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

(* Skip annotations *)
let annotations =
  (any (elt "annotation" (a2 (ars "name") (ars "value"))
          s0
          (fun _ _ -> ())))

type direction = In | Out

let dir_in = [("in", In)]
let dir_out = [("out", Out)]
let dir_both = dir_in @ dir_out

let adname = ars "name"
let acname = aos "cname"
let adtype = ar "type" dbus_type_of_signature
let actype = ao "ctype" (type_of_string "unit")

let arguments dirs =
  any (elt "arg" (a5 (aos "name") (aos "cname") (afd "direction" In dirs) adtype actype)
         s0
         (fun dname cname dir dtyp ctyp ->
            (dir, (dname, cname, dtyp), mktyp dtyp ctyp)))

let remove_dir = List.map (fun (a, b, c) -> (b, c))

let method_decl =
  elt "method" (a3 adname acname actype)
    (s1 (arguments dir_both))
    (fun dname cname ctyp args ->
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
                    (string_of_type "unit" t))
           end
         | None -> (tuple in_ctyps, tuple out_ctyps)
       in
         Method(dname, mklid dname cname, (in_args, caml_in_typ), (out_args, caml_out_typ)))

let signal_decl =
  elt "signal" (a3 adname acname actype)
    (s1 (arguments dir_in))
    (fun dname cname ctyp args ->
       let args, ctyps = List.split (remove_dir args) in
         Signal(dname, mklid dname cname,
                (args,
                 match ctyp with
                   | Some t -> t
                   | None -> tuple ctyps)))

let proxy_decl =
  elt "proxy" (a4 adname (afd "type" `Bus ["bus", `Bus; "connection", `Connection])
                 (aos "destination") (aos "path"))
    s0
    (fun name typ dest path ->
       Proxy(name, typ, dest, path))

let flag_decl =
  elt "flag" (a1 (ars "name"))
    (s1 (any (elt "value" (a2 (ar "key" int_of_string) (ars "name"))
                s0
                (fun key name -> (key, name)))))
    (fun name values -> Flag(name, values))

let interface =
  elt "interface" (a2 adname acname)
    (s1 (union [method_decl;
                signal_decl;
                proxy_decl;
                flag_decl]))
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
