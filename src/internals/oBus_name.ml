(*
 * oBus_name.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open String
open OBus_string

type bus = string
type interface = string
type member = string
type error = string

(* +-----------------------------------------------------------------+
   | Bus names                                                       |
   +-----------------------------------------------------------------+ *)

let is_unique name = length name > 0 && unsafe_get name 0 = ':'

let validate_unique_connection str =
  let fail i msg = Some{ typ = "unique connection name"; str = str; ofs = i; msg = msg }
  and len = length str in

  let rec element_start i =
    if i = len then
      fail i "empty element"
    else match unsafe_get str i with
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' | '0' .. '9'->
          element (i + 1)
      | '.' ->
          fail i "empty element"
      | _ ->
          fail i "invalid character"

  and element i =
    if i = len then
      None
    else match unsafe_get str i with
      | '.' ->
          element_start (i + 1)
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' | '0' .. '9'->
          element (i + 1)
      | _ ->
          fail i "invalid character"

  and first_element i =
    if i = len then
      fail (-1) "must contains at least two elements"
    else match unsafe_get str i with
      | '.' ->
          element_start (i + 1)
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' | '0' .. '9'->
          first_element (i + 1)
      | _ ->
          fail i "invalid character"
  in

  if len > OBus_protocol.max_name_length then
    fail (-1) "name too long"
  else if len = 1 then
    fail 1 "premature end of name"
  else match unsafe_get str 1 with
    | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' | '0' .. '9'->
        first_element 2
    | '.' ->
        fail 1 "empty element"
    | _ ->
        fail 1 "invalid character"

let validate_bus_other str =
  let fail i msg = Some{ typ = "unique connection name"; str = str; ofs = i; msg = msg }
  and len = length str in

  let rec element_start i =
    if i = len then
      fail i "empty element"
    else match unsafe_get str i with
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' ->
          element (i + 1)
      | '.' ->
          fail i "empty element"
      | _ ->
          fail i "invalid character"

  and element i =
    if i = len then
      None
    else match unsafe_get str i with
      | '.' ->
          element_start (i + 1)
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' | '0' .. '9'->
          element (i + 1)
      | _ ->
          fail i "invalid character"

  and first_element i =
    if i = len then
      fail (-1) "must contains at least two elements"
    else match unsafe_get str i with
      | '.' ->
          element_start (i + 1)
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' | '0' .. '9'->
          first_element (i + 1)
      | _ ->
          fail i "invalid character"
  in

  if len > OBus_protocol.max_name_length then
    fail (-1) "name too long"
  else match unsafe_get str 1 with
    | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' | '0' .. '9'->
        first_element 2
    | '.' ->
        element_start 2
    | _ ->
        fail 1 "invalid character"

let validate_bus = function
  | "" ->
      Some{ typ = "bus name"; str = ""; ofs = -1; msg = "empty name" }
  | str ->
      match unsafe_get str 0 with
        | ':' -> validate_unique_connection str
        | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-' -> validate_bus_other str
        | '.' -> Some{ typ = "bus name"; str = str; ofs = 0; msg = "empty element" }
        | _ -> Some{ typ = "bus name"; str = str; ofs = 0; msg = "invalid character" }

(* +-----------------------------------------------------------------+
   | Interface names                                                 |
   +-----------------------------------------------------------------+ *)

let validate_interface str =
  let fail i msg = Some{ typ = "interface name"; str = str; ofs = i; msg = msg }
  and len = length str in

  let rec element_start i =
    if i = len then
      fail i "empty element"
    else match unsafe_get str i with
      | 'A' .. 'Z' | 'a' .. 'z' | '_' ->
          element (i + 1)
      | '.' ->
          fail i "empty element"
      | _ ->
          fail i "invalid character"

  and element i =
    if i = len then
      None
    else match unsafe_get str i with
      | '.' ->
          element_start (i + 1)
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' ->
          element (i + 1)
      | _ ->
          fail i "invalid character"

  and first_element i =
    if i = len then
      fail (-1) "must contains at least two elements"
    else match unsafe_get str i with
      | '.' ->
          element_start (i + 1)
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' ->
          first_element (i + 1)
      | _ ->
          fail i "invalid character"
  in

  if len > OBus_protocol.max_name_length then
    fail (-1) "name too long"
  else if len = 0 then
    fail (-1) "empty name"
  else match unsafe_get str 0 with
    | 'A' .. 'Z' | 'a' .. 'z' | '_' ->
        first_element 1
    | '.' ->
        fail 0 "empty element"
    | _ ->
        fail 0 "invalid character"

(* +-----------------------------------------------------------------+
   | Member names                                                    |
   +-----------------------------------------------------------------+ *)

let validate_member str =
  let fail i msg = Some{ typ = "member name"; str = str; ofs = i; msg = msg }
  and len = length str in

  let rec aux i =
    if i = len then
      None
    else match unsafe_get str i with
      | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' ->
          aux (i + 1)
      | _ ->
          fail i "invalid character"
  in

  if len > OBus_protocol.max_name_length then
    fail (-1) "name too long"
  else if len = 0 then
    fail (-1) "empty name"
  else match unsafe_get str 0 with
    | 'A' .. 'Z' | 'a' .. 'z' | '_' ->
        aux 1
    | _ ->
        fail 0 "invalid character"

(* +-----------------------------------------------------------------+
   | Error names                                                     |
   +-----------------------------------------------------------------+ *)

let validate_error str =
  (* Error names have the same restriction as interface names *)
  match validate_interface str with
    | None ->
        None
    | Some error ->
        Some{ error with typ = "error name" }

(* +-----------------------------------------------------------------+
   | Name translation                                                |
   +-----------------------------------------------------------------+ *)

(* Split a name into blocks. Blocks are the longest sub-strings
   matched by the regulare expression: "[A-Z]*[^A-Z.]*" *)
let split name =

  (* Recognize the first part of a block: "[A-Z]*" *)
  let rec part1 i =
    if i = String.length name then
      i
    else
      match name.[i] with
        | 'A' .. 'Z' ->
            part1 (i + 1)
        | _ ->
            part2 i

  (* Recognize the second part of a block: "[^A-Z.]*" *)
  and part2 i =
    if i = String.length name then
      i
    else
      match name.[i] with
        | 'A' .. 'Z' | '.' ->
            i
        | _ ->
            part2 (i + 1)

  in

  let rec split i =
    if i = String.length name then
      []
    else
      let j = part1 i in
      if j = i then
        (* Skip empty blocks *)
        split (i + 1)
      else
        String.sub name i (j - i) :: split j

  in
  split 0

let ocaml_lid name = String.uncapitalize_ascii (String.concat "_" (List.map String.lowercase_ascii (split name)))
let ocaml_uid name = String.capitalize_ascii (String.concat "_" (List.map String.lowercase_ascii (split name)))

let haskell_lid name = String.uncapitalize_ascii (String.concat "" (split name))
let haskell_uid name = String.capitalize_ascii (String.concat "" (split name))
