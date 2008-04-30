(*
 * util.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
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

let rec ljoin sep strs = String.concat sep strs

let rec rjoin sep strs = String.concat sep (List.rev strs)

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

let part_map f l =
  List.fold_right (fun x (success, failure) -> match f x with
                     | None -> (success, x :: failure)
                     | Some(v) -> (v :: success, failure)) l ([], [])

let xml_parser = XmlParser.make ()
let _ = XmlParser.prove xml_parser false

let parse_xml fname =
  XmlParser.parse xml_parser (XmlParser.SFile fname)

let gen_names prefix l =
  List.rev (snd (List.fold_left (fun (i, acc) _ -> (i + 1, (prefix ^ string_of_int i) :: acc)) (0, []) l))

