(*
 * oBus_path.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open String

type t = string
type elt = string

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
  | "" -> raise (Invalid_argument "invalid path")
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
  | _ -> raise (Invalid_argument "invalid path")
