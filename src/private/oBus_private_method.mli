(*
 * oBus_private_method.mli
 * -----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Helpers for calling methods *)

val invalid_reply : (string -> exn) ref
  (** This variable is set in {!OBus_method}. We use a reference to
      avoid circular dependencies. *)

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
  o_args : 'b OBus_value.C.sequence -> 'a -> (unit OBus_private_connection.context * 'b) Lwt.t

val call_no_reply :
  connection : OBus_connection.t ->
  path : OBus_path.t ->
  ?destination : OBus_name.bus ->
  interface : OBus_name.interface ->
  member : OBus_name.member ->
  i_args : 'a OBus_value.C.sequence -> 'a -> unit Lwt.t
