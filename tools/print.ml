(*
 * print.ml
 * --------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Code printers *)

open Format
open OBus_introspect
open Term

let translator : [ `ocaml | `haskell ] ref = ref `ocaml

let plid pp str = match !translator with
  | `ocaml -> pp_print_string pp (OBus_name.ocaml_lid str)
  | `haskell -> pp_print_string pp (OBus_name.haskell_lid str)

let puid pp str = match !translator with
  | `ocaml -> pp_print_string pp (OBus_name.ocaml_uid str)
  | `haskell -> pp_print_string pp (OBus_name.haskell_uid str)

let unit = term "unit" []

let str_of_access = function
  | Read -> "r"
  | Write -> "w"
  | Read_write -> "rw"

(* +-----------------------------------------------------------------+
   | Printing of proxy code signature                                |
   +-----------------------------------------------------------------+ *)

let if_term_of_args = List.map (fun (name, typ) -> (name, interf_term_of_single typ))

let print_proxy_interf pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a : sig\n" puid name;
  p "  type t = OBus_proxy.t with obus(basic)\n";
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  val %a : %a\n" plid name
          (print_func (term "Lwt.t" [tuple (List.map snd (if_term_of_args  outs))]))
          ((None, term "t" []) :: if_term_of_args ins)
    | Signal(name, args, annots) ->
        p "  val %a : t -> %a\n" plid name
          (print_term true)
          (term "OBus_signal.t"
             [match args with
                | [] -> unit
                | _ -> tuple (List.map snd (if_term_of_args args))])
    | Property(name, typ, access, annots) ->
        p "  val %a : t -> %a\n" plid name
          (print_term true)
          (term
             (match access with
                | Read -> "OBus_property.r"
                | Write -> "OBus_property.w"
                | Read_write -> "OBus_property.rw")
             [interf_term_of_single typ])
  end content;
  p "end\n"

(* +-----------------------------------------------------------------+
   | Printing of proxy code structure                                |
   +-----------------------------------------------------------------+ *)

let im_term_of_args = List.map (fun (name, typ) -> (name, implem_term_of_single typ))

let print_proxy_implem pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a = struct\n" puid name;
  p "  type t = OBus_proxy.t with obus\n";
  p "  let op_interface = OBus_proxy.make_interface %S\n" name;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  OP_method %s : %a\n" name (print_func (tuple (List.map snd (im_term_of_args  outs)))) (im_term_of_args ins)
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (List.map snd (im_term_of_args args))
        in
        p "  OP_signal %s : %a\n" name (print_term false) args
    | Property(name, typ, access, annots) ->
        p "  OP_property_%s %s : %a\n" (str_of_access access) name (print_term false) (implem_term_of_single typ)
  end content;
  p "end\n"

(* +-----------------------------------------------------------------+
   | Printing of service code virtual classes                        |
   +-----------------------------------------------------------------+ *)

let print_service_implem pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a = struct\n" puid name;
  p "  let ol_interface = M.make_interface %S\n" name;
  List.iter begin function
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (List.map snd (if_term_of_args args))
        in
        p "  OL_signal %s : %a\n" name
          (print_term true) args
    | _ ->
        ()
  end content;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  let %a obj" plid name;
        ignore (List.fold_left (fun i (name, typ) ->
                                  match name with
                                    | Some name ->
                                        p " %a" plid name;
                                        i
                                    | None ->
                                        p " x%d" i;
                                        i + 1) 0 ins);
        p " = Lwt.fail (Failure \"not imlemented\")\n";
    | Property(name, typ, access, annots) ->
        if access = Read || access = Read_write then
          p "  let %a obj = Lwt.fail (Failure \"not imlemented\")\n" plid name;
        if access = Write || access = Read_write then
          p "  let %a obj x = Lwt.fail (Failure \"not imlemented\")\n" plid ("set" ^ name)
    | _ ->
        ()
  end content;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  OL_method %s : %a\n" name
          (print_func (tuple (List.map snd (im_term_of_args  outs))))
          (im_term_of_args ins)
    | Property(name, typ, access, annots) ->
        p "  OL_property_%s %s : %a\n" (match access with
                                               | Read -> "r"
                                               | Write -> "w"
                                               | Read_write -> "rw")
          name (print_term true) (interf_term_of_single typ)
    | _ ->
        ()
  end content;
  p "end\n"
