(*
 * oBus_context.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_private_connection

type 'a t = 'a OBus_private_connection.context

let make ~connection ~message = {
  context_connection = connection;
  context_message = message;
  context_arguments = OBus_value.arg0;
  context_replied = false;
}

let make_with_arguments ~connection ~message ~arguments = {
  context_connection = connection;
  context_message = message;
  context_arguments = arguments;
  context_replied = false;
}

let connection ctx = ctx.context_connection
let message ctx = ctx.context_message
let arguments ctx = ctx.context_arguments

let replied ctx = ctx.context_replied
let set_replied ctx = ctx.context_replied <- true

let sender ctx = {
  OBus_peer.connection = ctx.context_connection;
  OBus_peer.name = OBus_message.sender ctx.context_message;
}
