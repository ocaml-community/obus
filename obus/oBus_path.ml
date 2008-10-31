(*
 * oBus_path.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open String

type element = string
type t = element list

exception Invalid_path of string * string
exception Invalid_element of string * string

let invalid_char str i =
  sprintf "at position %d: invalid character %C, must be one of \"[A-Z][a-z][0-9]_\"" i str.[i]

let is_valid_char ch =
  (ch >= 'A' && ch <= 'Z') ||
    (ch >= 'a' && ch <= 'z') ||
    (ch >= '0' && ch <= '9') ||
    ch = '_'

let test str =
  let len = length str in
  let rec aux_member_start i =
    if i = len
    then
      (if str.[i-1] = '/'
       then Some "trailing '/'"
       else Some(sprintf "at position %d: empty member" i))
    else
      if is_valid_char (unsafe_get str i)
      then aux_member (i + 1)
      else
        if unsafe_get str i = '/'
        then Some(sprintf "at position %d: empty member" i)
        else Some(invalid_char str i)
  and aux_member i =
    if i = len
    then None
    else match unsafe_get str i with
      | '/' -> aux_member_start (i + 1)
      | ch when is_valid_char ch -> aux_member (i + 1)
      | ch -> Some(invalid_char str i)
  in
  if len = 0
  then Some "empty path"
  else match unsafe_get str 0 with
    | '/' -> if len = 1 then None else aux_member_start 1
    | _ -> Some "must start with '/'"

let test_element = function
  | "" -> Some("empty member")
  | str ->
      let rec aux = function
        | -1 -> None
        | i ->
            if is_valid_char (unsafe_get str i)
            then aux (i - 1)
            else Some(invalid_char str i)
      in
      aux (length str - 1)

let validate str = match test str with
  | None -> ()
  | Some msg -> raise (Invalid_path(str, msg))

let validate_element str = match test_element str with
  | None -> ()
  | Some msg -> raise (Invalid_element(str, msg))

let empty = []

let to_string = function
  | [] -> "/"
  | path ->
      let str = create (List.fold_left (fun len elt -> len + length elt + 1) 0 path) in
      ignore
        (List.fold_left
           (fun pos elt ->
              unsafe_set str pos '/';
              let len = length elt in
              unsafe_blit elt 0 str (pos + 1) len;
              pos + 1 + len)
           0 path);
      str

let of_string = function
  | "" -> raise (Invalid_path("", "empty path"))
  | "/" -> []
  | str when unsafe_get str 0 = '/' ->
      let rec aux acc = function
        | -1 -> acc
        | j ->
            let i = rindex_from str j '/' in
            let len = j - i in
            let elt = create len in
            unsafe_blit str (i + 1) elt 0 len;
            match test_element elt with
              | Some _ -> validate str; assert false
              | None -> aux (elt :: acc) (i - 1)
      in
      aux [] (length str - 1)
  | s -> validate s; assert false

let escape s =
  let len = length s in
  let r = create (len * 2) in
  for i = 0 to len - 1 do
    let j = i * 2 in
    r.[j] <- char_of_int (int_of_char s.[i] land 15 + int_of_char 'a');
    r.[j] <- char_of_int (int_of_char s.[i + 1] lsr 4 + int_of_char 'a')
  done;
  r

let unescape s =
  let len = length s / 2 in
  let r = create len in
  for i = 0 to len - 1 do
    let j = i * 2 in
    r.[i] <- char_of_int ((int_of_char s.[j] - int_of_char 'a') lor
                            ((int_of_char s.[j + 1] - int_of_char 'a') lsl 4))
  done;
  r

let rec after prefix path = match prefix, path with
  | [], p -> Some p
  | e1 :: p1, e2 :: p2 when e1 = e2 -> after p1 p2
  | _ -> None
