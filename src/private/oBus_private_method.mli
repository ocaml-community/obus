(*
 * oBus_private_method.mli
 * -----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Helpers for calling methods *)

exception Invalid_reply of string

val call :
  connection : OBus_connection.t ->
  path : OBus_path.t ->
  ?destination : OBus_name.bus ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence ->
  o_args : 'b OBus_value.C.sequence -> 'a -> 'b Lwt.t

val call_with_context :
  connection : OBus_connection.t ->
  path : OBus_path.t ->
  ?destination : OBus_name.bus ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence ->
  o_args : 'b OBus_value.C.sequence -> 'a -> (OBus_private_connection.void OBus_private_connection.context * 'b) Lwt.t

val call_no_reply :
  connection : OBus_connection.t ->
  path : OBus_path.t ->
  ?destination : OBus_name.bus ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence -> 'a -> unit Lwt.t
