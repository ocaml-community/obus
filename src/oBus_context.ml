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
type void = OBus_private_connection.void

let make = OBus_private_connection.make_context
let make_with_reply = OBus_private_connection.make_context_with_reply

let connection ctx = ctx.context_connection
let flags ctx = ctx.context_flags
let serial ctx = ctx.context_serial

let sender ctx = {
  OBus_peer.connection = ctx.context_connection;
  OBus_peer.name = ctx.context_sender;
}

let destination ctx = {
  OBus_peer.connection = ctx.context_connection;
  OBus_peer.name = ctx.context_destination;
}

let map f ctx = {
  ctx with
    context_make_body = (fun x -> ctx.context_make_body (f x));
}

let replied ctx =
  !(ctx.context_replied)

let set_replied ctx =
  ctx.context_replied := true

let make_body ctx x =
  ctx.context_make_body x
