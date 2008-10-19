(*
 * name_translator.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Translation DBus name -> Caml name *)

open Format

module type Translator = sig
  val plid : Format.formatter -> string -> unit
  val puid : Format.formatter -> string -> unit
end

module Lower : Translator =
struct
  (* "SetCPUFreqGovernor" -> "set_cpufreq_governor"
     "org.freedesktop.DBus" -> "Org_freedesktop_dbus" *)

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
end

module Upper : Translator =
struct
  (* "SetCPUFreqGovernor" -> "setCPUFreqGovernor"
     "org.freedesktop.DBus" -> "OrgFreedesktopDBus" *)

  let plid pp str = pp_print_string pp (String.uncapitalize str)

  let rec split str i =
    if i >= String.length str then
      []
    else
      let j = try String.index_from str i '.' with _ -> String.length str in
      String.sub str i (j - i) :: split str (j + 1)

  let puid pp str =
    List.iter (fun x -> pp_print_string pp (String.capitalize x))
      (split str 0)
end
