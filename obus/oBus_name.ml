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

exception Invalid_name of string * string * string

module type Name = sig
  type t = string
  val test : string -> string option
  val validate : string -> unit
end

module type Specs = sig
  val typ : string
  val range : string
  val is_valid_char : char -> bool
end

let is_digit ch = ch >= '0' && ch <= '9'

module Default(S : Specs) = struct
  include S

  type t = string

  let invalid_char str i =
    sprintf "at position %d: invalid character %C, must be one of [0-9]%S" i str.[i] range
  let invalid_first_char str i =
    sprintf "at position %d: invalid character %C at begining of member, must be one of %S" i str.[i] range

  let _test str i =
    let len = String.length str in
    let rec aux_member_start i =
      if i = len
      then Some(sprintf "at position %d: empty member" i)
      else
        if is_valid_char (unsafe_get str i)
        then aux_member (i + 1)
        else
          if unsafe_get str i = '.'
          then Some(sprintf "at position %d: empty member" i)
          else Some(invalid_first_char str i)
    and aux_member i =
      if i = len
      then None
      else match unsafe_get str i with
        | '.' -> aux_member_start (i + 1)
        | ch when is_valid_char ch || is_digit ch -> aux_member (i + 1)
        | ch -> Some(invalid_char str i)
    in
    if len > OBus_info.max_name_length
    then Some "name too long"
    else aux_member_start i

  let test = function
    | "" -> Some "empty len"
    | s -> _test s 0

  let _validate x i = match _test x i with
    | None -> ()
    | Some msg -> raise (Invalid_name(typ, x, msg))

  let validate x = match test x with
    | None -> ()
    | Some msg -> raise (Invalid_name(typ, x, msg))
end

module Interface = Default(struct
                             let typ = "interface"
                             let range = "[A-Z][a-z]_"
                             let is_valid_char ch =
                               (ch >= 'A' && ch <= 'Z') ||
                                 (ch >= 'a' && ch <= 'z') ||
                                 ch = '_'
                           end)

module Error = Default(struct
                         let typ = "error"
                         let range = Interface.range
                         let is_valid_char = Interface.is_valid_char
                       end)

module Bus = Default(struct
                       let typ = "bus"
                       let range = "[A-Z][a-z]_-"
                       let is_valid_char ch =
                         (ch >= 'A' && ch <= 'Z') ||
                           (ch >= 'a' && ch <= 'z') ||
                           ch = '_' || ch = '-'
                     end)

module Connection_unique = struct
  include Default(struct
                    let typ = "connection unique"
                    let range = Bus.range
                    let is_valid_char ch = Bus.is_valid_char ch || is_digit ch
                  end)
  let test = function
    | "" -> Some "empty name"
    | s when unsafe_get s 0 = ':' -> _test s 1
    | _ -> Some "must start with ':'"
  let validate x = match test x with
    | None -> ()
    | Some msg -> raise (Invalid_name(typ, x, msg))
end

module Connection = struct
  type t = string
  let test = function
    | "" -> Some "empty name"
    | s when unsafe_get s 0 = ':' -> Connection_unique._test s 1
    | s -> Bus._test s 0
  let validate x = match test x with
    | None -> ()
    | Some msg -> raise (Invalid_name("connection", x, msg))
end

module Member = struct
  type t = string
  let is_valid_first_char ch =
    (ch >= 'A' && ch <= 'Z') ||
      (ch >= 'a' && ch <= 'z') ||
      ch = '_'
  let is_valid_char ch = is_valid_first_char ch || is_digit ch
  let test str =
    let len = String.length str in
    let rec aux i =
      if i = len
      then None
      else
        if is_valid_char (unsafe_get str i)
        then aux (i + 1)
        else Some(sprintf "at position %d: invalid character %C, must be one of [0-9][A-Z][a-z]_" i str.[i])
    in
    if len = 0
    then Some "empty name"
    else
      if len > OBus_info.max_name_length
      then Some "name too long"
      else
        if is_valid_first_char (unsafe_get str 0)
        then aux 1
        else Some(sprintf "at position 0: invalid character %C, must be one of [A-Z][a-z]_" str.[0])
  let validate x = match test x with
    | None -> ()
    | Some msg -> raise (Invalid_name("member", x, msg))
end

let is_unique_connection_name s = s <> "" && unsafe_get s 0 = ':'
let is_bus_name s = not (is_unique_connection_name s)

type connection_unique = Connection_unique.t
type bus = Bus.t
type connection = Connection.t
type interface = Interface.t
type member = Member.t
type error = Error.t
