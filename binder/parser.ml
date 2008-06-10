(*
 * parser.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types
open Xparser
open Introspect

let mklid did = function
  | None -> StrUtil.camlize_lid did
  | Some cid -> cid

let mkuids did = function
  | None -> List.map String.capitalize (StrUtil.split_dot did)
  | Some cid -> StrUtil.split_dot cid

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
let adtype = ar "type" dtype_of_signature
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
             | t :: ts -> (List.rev ts, list_of_tuple t)
             | _ -> failwith
                 (Printf.sprintf
                    "invalid caml type for a method: %s, must have the form 'a -> 'b"
                    (string_of_caml_type t))
           end
         | None -> (in_ctyps, out_ctyps)
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
                   | Some t -> list_of_tuple t
                   | None -> ctyps)))

let convert_decl =
  elt "type" (a4
                   (ar "new" caml_type_of_string)
                   (ar "old" caml_type_of_string)
                   (aos "new_of_old")
                   (aos "old_of_new"))
    s0
    (fun a b a_of_b b_of_a ->
       Convert(a, b, a_of_b, b_of_a))

let error_decl =
  elt "error" (a2 adname acname)
    (s1 doc)
    (fun dname cname doc ->
       Exception(doc, dname,
                 match cname with
                   | Some n -> n
                   | None ->
                       match List.rev (StrUtil.split_dot dname) with
                         | [] -> failwith "null error name"
                         | n :: _ ->
                             String.capitalize (StrUtil.camlize_lid n)))

let interf_decl =
  elt "interf" (a1 (abf "implem"))
    (s1 (one text))
    (fun is_implem interf -> Interf(interf, is_implem))

let implem_decl =
  elt "implem" a0
    (s1 (one text))
    (fun implem -> Implem(implem))

let doc_decl =
  elt "doc" a0 (s1 (any text)) (fun x -> Doc x)

let interface =
  elt "interface" (a4 adname acname (ad "proxy" (typ "Proxy.t" [typ "t" []]) caml_type_of_string) (aos "to_proxy"))
    (s1 (union [method_decl;
                signal_decl;
                convert_decl;
                error_decl;
                interf_decl;
                implem_decl;
                doc_decl]))
    (fun dname cname proxy_typ to_proxy decls ->
       (dname, mkuids dname cname, decls, proxy_typ, to_proxy))

let document =
  elt "node" a0
    (s2
       (any interface)
       (any (elt "node" a0
               s0
               ())))
    (fun interfs _ -> interfs)
