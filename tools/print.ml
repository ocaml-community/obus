(*
 * print.ml
 * --------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
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

(* +----------------------------------+
   | Printing of proxy code signature |
   +----------------------------------+ *)

let if_term_of_args = List.map (fun (name, typ) -> interf_term_of_single typ)

let print_proxy_interf pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a : sig\n" puid name;
  p "  type t = OBus_proxy.t\n";
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  val %a : %a\n" plid name
          (print_func (term "Lwt.t" [tuple (if_term_of_args  outs)]))
          (term "t" [] :: if_term_of_args ins)
    | Signal(name, args, annots) ->
        p "  val %a : t -> %a\n" plid name
          (print_term true)
          (term "OBus_signal.t"
             [match args with
                | [] -> unit
                | _ -> tuple (if_term_of_args args)])
    | Property(name, typ, access, annots) ->
        p "  val %a : t -> %a\n" plid name
          (print_term true)
          (term "OBus_property.t"
             [interf_term_of_single typ;
              term
                (match access with
                   | Read -> "[ `readable ]"
                   | Write -> "[ `writable ]"
                   | Read_write -> "[ `readable | `writable ]") []])
  end content;
  p "end\n"

(* +----------------------------------+
   | Printing of proxy code structure |
   +----------------------------------+ *)

let im_term_of_args = List.map (fun (name, typ) -> implem_term_of_single typ)

let print_proxy_implem pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "module %a = struct\n" puid name;
  p "  include OBus_interface.Make_proxy(struct let name = %S end)\n" name;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  OBUS_method %s : %a\n" name (print_func (tuple (im_term_of_args  outs))) (im_term_of_args ins)
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (im_term_of_args args)
        in
        p "  OBUS_signal %s : %a\n" name (print_term false) args
    | Property(name, typ, access, annots) ->
        p "  OBUS_property_%s %s : %a\n" (str_of_access access) name (print_term false) (implem_term_of_single typ)
  end content;
  p "end\n"

(* +------------------------------------------+
   | Printing of service code virtual classes |
   +------------------------------------------+ *)

let print_service_implem pp (name, content, annots) =
  let p fmt = fprintf pp fmt in
  p "class virtual %a = OBUS_interface %S\n" plid name name;
  List.iter begin function
    | Method(name, ins, outs, annots) ->
        p "  OBUS_method %s : %a\n" name
          (print_func (tuple (if_term_of_args  outs)))
          (if_term_of_args ins)
    | Signal(name, args, annots) ->
        let args = match args with
          | [] -> unit
          | _ -> tuple (if_term_of_args args)
        in
        p "  OBUS_signal %s : %a\n" name
          (print_term true) args
    | Property(name, typ, access, annots) ->
        p "  OBUS_property_%s %s : %a\n" (match access with
                                               | Read -> "r"
                                               | Write -> "w"
                                               | Read_write -> "rw")
          name (print_term true) (interf_term_of_single typ)
  end content;
  p "end\n"
