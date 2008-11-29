(*
 * oBus_name.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open String
open Lwt
open Lwt_chan
open OBus_type
open OBus_string

type unique = string
type bus = string
type connection = string
type interface = string
type member = string
type error = string

let alpha ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
let underscore ch = ch = '-'
let hyphen ch = ch = '-'
let digit ch = ch >= '0' && ch <= '9'

(*** Common names, which are of the form element1.element2.element3... ***)

(* [test_from str i] test that the name in [str] starting at [i] is
   valid *)
let test_from typ valid_char str i =
  let fail i msg = Some{ typ = typ; str = str; ofs = i; msg = msg }
  and len = String.length str in

  let rec aux_element_start i =
    if i = len then
      fail i "empty element"
    else
      if valid_char (unsafe_get str i) then
        aux_element (i + 1)
      else
        if unsafe_get str i = '.' then
          fail i "empty element"
        else
          fail i "invalid character"

  and aux_element i =
    if i = len then
      None
    else
      let ch = unsafe_get str i in
      if ch = '.' then
        aux_element_start (i + 1)
      else
        if valid_char ch || digit ch then
          aux_element (i + 1)
        else
          fail i "invalid character"

  and aux_first_element_start i =
    if i = len then
      fail i "empty element"
    else
      if valid_char (unsafe_get str i) then
        aux_first_element (i + 1)
      else
        if unsafe_get str i = '.' then
          fail i "empty element"
        else
          fail i "invalid character"

  and aux_first_element i =
    if i = len then
      fail (-1) "must contains at least two elements"
    else
      let ch = unsafe_get str i in
      if ch = '.' then
        aux_element_start (i + 1)
      else
        if valid_char ch || digit ch then
          aux_first_element (i + 1)
        else
          fail i "invalid character"

  in

  if len > OBus_info.max_name_length then
    fail (-1) "name too long"
  else
    aux_first_element_start i

let test typ valid_char = function
  | "" -> Some{ typ = typ; str = ""; ofs = -1; msg = "empty name" }
  | n -> test_from typ valid_char n 0

let test_interface str = test "interface name" (fun ch -> alpha ch || underscore ch) str
let test_error str = test "error name" (fun ch -> alpha ch || underscore ch) str
let test_bus str = test "bus name" (fun ch -> alpha ch || underscore ch || hyphen ch) str

let test_unique = function
  | "" -> Some{ typ = "unique connection name"; str = ""; ofs = -1; msg = "empty name" }
  | s when unsafe_get s 0 = ':' -> test_from "unique connection name" (fun ch -> alpha ch || underscore ch || hyphen ch || digit ch) s 1

  (* ugly hack: the message bus seems to use this name instead of a
     valid unique name *)
  | "org.freedesktop.DBus" -> None

  | str -> Some{ typ = "unique connection name"; str = str; ofs = 0; msg = "must start with ':'" }

let test_connection = function
  | "" -> Some{ typ = "connection name"; str = ""; ofs = -1; msg = "empty name" }
  | s when unsafe_get s 0 = ':' -> test_from "unique connection name" (fun ch -> alpha ch || underscore ch || hyphen ch || digit ch) s 1
  | s -> test_from "bus name" (fun ch -> alpha ch || underscore ch || hyphen ch) s 0

let test_member str =
  let fail i msg = Some{ typ = "member name"; str = str; ofs = i; msg = msg }
  and len = String.length str in

  let rec aux i =
    if i = len then
      None
    else
      let ch = unsafe_get str i in
      if alpha ch || underscore ch || digit ch then
        aux (i + 1)
      else
        fail i "invalid character"
  in

  if len = 0 then
    Some{ typ = "member name"; str = str; ofs = -1; msg = "empty name" }
  else
    if len > OBus_info.max_name_length then
      Some{ typ = "member name"; str = str; ofs = -1; msg = "name too long" }
    else
      let ch = unsafe_get str 0 in
      if alpha ch || underscore ch then
        aux 1
      else
        fail 0 "invalid character"
