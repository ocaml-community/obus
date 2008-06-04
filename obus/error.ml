(*
 * error.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

exception DBus of string * string option

open Printf

let to_string = function
  | DBus(error, None) ->
      sprintf "DBus error, name='%s'" error
  | DBus(error, Some msg) ->
      sprintf "DBus error, name='%s', message='%s'" error msg
  | Transport.Error(msg, exn) ->
      sprintf "transport: %s%s" msg
        (match exn with
           | None -> ""
           | Some exn ->
               sprintf ", orignal error: %s" (Printexc.to_string exn))
  | Wire.Content_error msg ->
      sprintf "unexpected datas where found in the message: %s" msg
  | Wire.Convertion_failed exn ->
      sprintf "failed to convert some values, reason: %s" (Printexc.to_string exn)
  | Wire.Reading_error msg ->
      sprintf "invalid dbus message, reason: %s" msg
  | Wire.Writing_error msg ->
      sprintf "message serialization failed, reason: %s" msg
  | _ -> ""

open Header
open Wire

let get_error header buffer ptr =
  let msg = match header.fields.signature with
    | Some s when s <> "" && s.[0] = 's' ->
        Some(snd ((match header.byte_order with
                     | Little_endian -> LEReader.read_string_string
                     | Big_endian -> BEReader.read_string_string) buffer ptr))
    | _ -> None
  and name = match header.fields.error_name with
    | Some name -> name
    | _ -> "<unamed>"
  in
    (name, msg)

let raise_error header buffer ptr =
  let name, msg = get_error header buffer ptr in
    raise (DBus(name, msg))
