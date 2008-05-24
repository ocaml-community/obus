(*
 * codeConstants.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Types
open Helpers

let rec fixed_reader caml_type dbus_type idx =
  match caml_type, dbus_type with
    | "char", "byte" -> (<:expr< String.unsafe_get buffer $idx$ >>)
    | "int", "byte" -> (<:expr< Char.code (String.unsafe_get buffer $idx$) >>)
    | "int", "boolean" -> (<:expr< int_uint32 buffer $idx$ >>)
    | _ -> (<:expr< $lid:caml_type ^ "_" ^ dbus_type$ buffer $idx$ >>)

let rec fixed_writer caml_type dbus_type idx expr =
  match caml_type, dbus_type with
    | "char", "byte" -> (<:expr< String.unsafe_set buffer $idx$ $expr$ >>)
    | "int", "byte" -> (<:expr< String.unsafe_set buffer $idx$ (Char.unsafe_chr $expr$) >>)
    | "int", "boolean" -> (<:expr< int_uint32 buffer $idx$ $expr$ >>)
    | _ -> (<:expr< $lid:caml_type ^ "_" ^ dbus_type$ buffer  $idx$ $expr$ >>)

let string_reader buf idx len =
  (<:expr< String.unsafe_blit buffer $idx$ $buf$ 0 $len$ >>)

let string_writer buf idx len =
  (<:expr< String.unsafe_blit $buf$ 0 buffer $idx$ $len$ >>)

let signature_checker signature idx =
  let len = String.length signature in
  let sig_expr = expr_of_str (Printf.sprintf "%c%s\x00" (char_of_int len) signature) in
    (<:expr<
       if not string_match buffer $idx$ $sig_expr$ $expr_of_int (len + 2)$
       then raise Reading.Invalid_signature
         >>)

let realloc_buffer = (<:expr< Internal.realloc_buffer buffer i >>)
