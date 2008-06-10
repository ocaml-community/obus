(*
 * strUtil.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let split_upper name =
  let len = String.length name in
  let rec find_end_word previous_is_upper i =
    if i = len
    then i
    else let ch = name.[i] in
      match previous_is_upper, ch >= 'A' && ch <= 'Z' with
        | true, true -> find_end_word true (i + 1)
        | true, false -> find_end_word false (i + 1)
        | false, true -> i
        | false, false -> find_end_word false (i + 1)
  in
  let rec split i =
    if i = len
    then []
    else let j = find_end_word true (i + 1) in
      String.lowercase (String.sub name i (j - i)) :: split j
  in
    split 0

let dot_regexp = Str.regexp "\\."
let split_dot str = Str.split dot_regexp str
let newline_regexp = Str.regexp "\n"
let split_lines str = Str.split newline_regexp str

let gen_names prefix l =
  List.rev (snd (List.fold_left (fun (i, acc) _ -> (i + 1, (prefix ^ string_of_int i) :: acc)) (0, []) l))

let camlize_lid str = String.concat "_" (split_upper str)
let camlize_uid str = String.capitalize (String.concat "_" (split_upper str))
