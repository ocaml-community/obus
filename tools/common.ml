(*
 * common.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Format

open OBus_interface

(***** Name camlization *****)

(* Apply the following transformations:

   "SetCPUFreqGovernor" -> ["set"; "cpufreq"; "governor"]
   "org.freedesktop.DBus" -> ["org"; "freedesktop"; "dbus"] *)
let split name =
  let len = String.length name in
  let rec find_end_word previous_is_upper i =
    if i = len
    then (i, i)
    else match name.[i] with
      | '.' -> (i, i + 1)
      | ch when ch >= 'A' && ch <= 'Z' -> begin
          match previous_is_upper with
            | true -> find_end_word true (i + 1)
            | false -> (i, i)
        end
      | _ -> find_end_word false (i + 1)
  in
  let rec split i =
    if i = len
    then []
    else
      let j, k = find_end_word true (i + 1) in
      String.lowercase (String.sub name i (j - i)) :: split k
  in
  split 0

let rec print_parts pp = function
  | [] -> ()
  | [e] -> pp_print_string pp e
  | e :: l -> fprintf pp "%s_%a" e print_parts l

let plid pp str = print_parts pp (split str)
let puid pp str = match split str with
  | [] -> ()
  | e :: l -> print_parts pp (String.capitalize e :: l)

(***** Printing of module interfaces *****)

(* We do not use camlp4 because we must print implementation using the
   syntax extension *)

open Term

let unit = term "unit" []

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
        p "  val on_%a : t -> (%a) -> OBus_signal.receiver Lwt.t\n" plid name
          (print_func unit)
          (match args with
             | [] -> [unit]
             | _ -> if_term_of_args args)
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
