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

type t = string
type elt = string

exception Invalid_path of string * string

let invalid_char str i =
  sprintf "at position %d: invalid character %C, must be one of \"[A-Z][a-z][0-9]_\"" i str.[i]

let is_valid_char ch =
  (ch >= 'A' && ch <= 'Z') ||
    (ch >= 'a' && ch <= 'z') ||
    (ch >= '0' && ch <= '9') ||
    ch = '_'

let test str =
  let len = String.length str in
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

let validate str = match test str with
  | None -> ()
  | Some msg -> raise (Invalid_path(str, msg))

let empty = "/"

let append path elt = match path with
  | "/" -> "/" ^ elt
  | _ ->
      let path_len = length path
      and elt_len = length elt in
      let result = create (path_len + elt_len + 1) in
        unsafe_blit path 0 result 0 path_len;
        unsafe_set result path_len '/';
        unsafe_blit elt 0 result (path_len + 1) elt_len;
        result

let (/) = append

let make elts =
  let path = create (List.fold_left (fun len elt -> len + length elt + 1) 0 elts)  in
    ignore
      (List.fold_left
         (fun pos elt ->
            unsafe_set path pos '/';
            let len = length elt in
              unsafe_blit elt 0 path (pos + 1) len;
              pos + 1 + len)
         0 elts);
    path

let split = function
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
              aux (elt :: acc) (i - 1)
      in
        aux [] (length str - 1)
  | s -> validate s; assert false
