(*
 * message.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Helpers for creating messages *)

type path = string
type destination = string
type member = string
type body = Values.values

type flags =
  | No_reply_expected
  | No_auto_start

open Connection

val method_call : flags list -> destination -> path -> Interface.name -> member -> body -> send_message
  (** [method_call destination path interface member body] create a method call
      message *)

val method_reply : Header.recv -> body -> send_message
  (** [method_reply header body] create a reply message for the
      following message *)

val error : Header.recv -> string -> string option -> send_message
  (** [error header error_name error_message] create a error message
      reply for the given message *)

val signal : path -> Interface.name -> member -> body -> send_message
  (** [signal path interface member body] create a signal message *)
