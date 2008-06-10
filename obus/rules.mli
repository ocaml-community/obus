(*
 * rules.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Rules for matching messages, used by DBus.add_match *)

type message_type =
  | Method_call
  | Method_return
  | Error
  | Signal

type rule =
    (** A single rule for matching a message *)
  | Type of message_type
      (** Match the message type *)
  | Sender of Connection.name
      (** Match the sender of the message *)
  | Interface of Interface.name
      (** Match message that come from this interface *)
  | Member of string
      (** Match the interface member *)
  | Path of Proxy.path
      (** Match the object patj *)
  | Destination of Connection.name
      (** Match the message destination *)
  | Arg of int * string
      (** [Arg(n, v)] Match any message that have at least [n]+1
          parameters and for which the [n]th argument is a string
          equal to [v] *)

type t = rule list
  (** A matching rule *)

val to_string : t -> string
  (** Return a string reprensenting the match rule *)
