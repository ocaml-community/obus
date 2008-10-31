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

exception Invalid_name of string * string * string

module type Name = sig
  type t = string
  val test : string -> string option
  val validate : string -> unit
end

let is_digit ch = ch >= '0' && ch <= '9'

module type Params = sig
  val typ : string
  val range : string
  val is_valid_char : char -> bool
end

(* Common names, which are of the form member1.member2.member3... *)
module Common(Params : Params) = struct
  type t = string

  open Params

  let invalid_char str i =
    sprintf "at position %d: invalid character %C, must be one of [0-9%s]" i str.[i] range
  let invalid_first_char str i =
    sprintf "at position %d: invalid character %C at begining of member, must be one of [%s]" i str.[i] range

  (* [test_from str i] test that the name in [str] starting at [i] is
     valid *)
  let test_from str i =
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
    | "" -> Some "empty name"
    | n -> test_from n 0

  let validate x = match test x with
    | None -> ()
    | Some msg -> raise (Invalid_name(typ, x, msg))
end

module Interface_params =
struct
  let typ = "interface"
  let range = "A-Za-z_"
  let is_valid_char ch =
    (ch >= 'A' && ch <= 'Z') ||
      (ch >= 'a' && ch <= 'z') ||
      ch = '_'
end

module Error_params =
struct
  let typ = "error"
  let range = Interface_params.range
  let is_valid_char = Interface_params.is_valid_char
end

module Bus_params =
struct
  let typ = "bus"
  let range = "A-Za-z_-"
  let is_valid_char ch =
    (ch >= 'A' && ch <= 'Z') ||
      (ch >= 'a' && ch <= 'z') ||
      ch = '_' || ch = '-'
end

module Interface = Common(Interface_params)
module Error = Common(Interface_params)
module Bus = Common(Bus_params)

module Unique = struct
  include Common(struct
                    let typ = "unique"
                    let range = Bus_params.range
                    let is_valid_char ch = Bus_params.is_valid_char ch || is_digit ch
                  end)

  let test = function
    | "" -> Some "empty name"
    | s when unsafe_get s 0 = ':' -> test_from s 1

    (* ugly hack: the message bus seems to use this name instead of a
       valid unique name *)
    | "org.freedesktop.DBus" -> None

    | _ -> Some "must start with ':'"

  let validate x = match test x with
    | None -> ()
    | Some msg -> raise (Invalid_name("unique", x, msg))
end

module Connection = struct
  type t = string

  let test = function
    | "" -> Some "empty name"
    | s when unsafe_get s 0 = ':' -> Unique.test_from s 1
    | s -> Bus.test_from s 0

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
        else Some(sprintf "at position %d: invalid character %C, must be one of [0-9A-Za-z_]" i str.[i])
    in
    if len = 0
    then Some "empty name"
    else
      if len > OBus_info.max_name_length
      then Some "name too long"
      else
        if is_valid_first_char (unsafe_get str 0)
        then aux 1
        else Some(sprintf "at position 0: invalid character %C, must be one of [A-Za-z_]" str.[0])

  let validate x = match test x with
    | None -> ()
    | Some msg -> raise (Invalid_name("member", x, msg))
end

type unique = Unique.t
type bus = Bus.t
type connection = Connection.t
type interface = Interface.t
type member = Member.t
type error = Error.t
