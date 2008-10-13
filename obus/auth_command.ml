(*
 * auth_command.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type data = string

type client_command =
  | Client_auth of (string * data option) option
  | Client_cancel
  | Client_begin
  | Client_data of data
  | Client_error of string

type server_command =
  | Server_rejected of string list
  | Server_ok of OBus_address.guid
  | Server_data of data
  | Server_error
