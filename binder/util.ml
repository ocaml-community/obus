(*
 * util.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, a ocaml implemtation of dbus.
 *)

let try_finally f close arg =
  let result =
    try
      f arg
    with
      | e -> close arg; raise e
  in
    close arg;
    result

let with_open_in fname f =
  try_finally f close_in (open_in fname)

let with_open_out fname f =
  try_finally f close_out (open_out fname)

let rec join strs sep = match strs with
  | [] -> ""
  | [s] -> s
  | s :: l -> s ^ sep ^ join l sep

let rec rjoin strs sep = match strs with
  | [] -> ""
  | [s] -> s
  | s :: l -> join l sep ^ sep ^ s

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
