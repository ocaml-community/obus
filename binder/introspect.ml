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
type argument = name * name * dbus_type
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

let mklid dstr = function
  | "" -> camlize_lid dstr
  | cstr -> cstr

let dot_regexp = Str.regexp "\\."

let mkuids dstr = function
  | "!" -> List.map String.capitalize (Str.split dot_regexp dstr)
  | cstr -> Str.split dot_regexp cstr

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
  | "" -> default_caml_type dtyp
  | ctyp -> type_of_string ctyp "unit"

let mkopt = function
  | "" -> None
  | s -> Some s

(* Skip annotations *)
let annotations =
  (any (elt "annotation" [< (P"name"); (P"value") >]
          s0
          (fun _ _ -> ())))

let arguments dir =
  (any (elt "arg" [< (D("name", "")); (D("cname", "")); (A("direction", "in", [dir])); (P"type"); (D("ctype", "")) >]
          s0
          (fun dname cname _ dtyp ctyp ->
             let dtyp = dbus_type_of_signature dtyp in
               ((dname, mklid dname cname, dtyp), mktyp dtyp ctyp))))

let method_decl =
  elt "method" [< (P"name"); (D("cname", "")); (D("ctype", "")) >]
    (s2
       (arguments "in")
       (arguments "out"))
    (fun dname cname ctyp ins outs ->
       let in_args = List.map fst ins
       and out_args = List.map fst outs in
       let (caml_in_typ, caml_out_typ) =
         if ctyp <> ""
         then match List.rev (list_of_tuple (type_of_string ctyp "unit")) with
           | t :: ts ->
               (tuple (List.rev ts), t)
           | _ -> failwith ("invalid caml type for a method: " ^ ctyp)
         else
           (tuple (List.map snd ins), tuple (List.map snd outs))
       in
         Method(dname, mklid dname cname, (in_args, caml_in_typ), (out_args, caml_out_typ)))

let signal_decl =
  elt "signal" [< (P"name"); (D("cname", "")); (D("ctype", "")) >]
    (s1
       (arguments "in"))
    (fun dname cname ctyp args ->
       Signal(dname, mklid dname cname,
              (List.map fst args,
               if ctyp <> ""
               then type_of_string ctyp "unit"
               else tuple (List.map snd args))))

let proxy_decl =
  elt "proxy" [< (P"name"); (F("type", ["bus"; "connection"])); (D("destination", "")); (D("path", "")) >]
    s0
    (fun name typ dest path ->
       Proxy(name, (match typ with
                      | "bus" -> `Bus
                      | "connection" -> `Connection
                      | _ -> assert false), mkopt dest, mkopt path))

let flag_decl =
  elt "flag" [< (P"name") >]
    (s1 (any (elt "value" [< (P"key"); (P"name") >]
                s0
                (fun key name -> (int_of_string key, name)))))
    (fun name values -> Flag(name, values))

let interface =
  elt "interface" [< (P"name"); (D("cname", "!")) >]
    (s1 (union [method_decl;
                signal_decl;
                proxy_decl;
                flag_decl]))
    (fun dname cname decls ->
       (dname, mkuids dname cname, decls))

let node =
  elt "node" [<>]
    (s2
       (any interface)
       (any (elt "node" [<>]
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

let parse_xmls xmls =
  let interfs = List.flatten (List.map (parse node) xmls) in
    List.fold_left insert (Node("", [], [])) interfs

let contain_dbus_declaration = List.exists (function
                                              | Method _
                                              | Signal _
                                              | Property _ -> true
                                              | _ -> false)
