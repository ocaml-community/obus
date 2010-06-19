(*
 * oBus_context.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_private_connection

type 'a t = 'a OBus_private_connection.message_context
type void = OBus_private_connection.void

let make = OBus_private_connection.make_context
let make_with_reply = OBus_private_connection.make_context_with_reply

let connection ctx = ctx.mc_connection
let flags ctx = ctx.mc_flags
let serial ctx = ctx.mc_serial

let sender ctx = {
  OBus_peer.connection = ctx.mc_connection;
  OBus_peer.name = ctx.mc_sender;
}

let destination ctx = {
  OBus_peer.connection = ctx.mc_connection;
  OBus_peer.name = ctx.mc_destination;
}

let map f ctx = {
  ctx with
    mc_make_body = (fun x -> ctx.mc_make_body (f x));
}

let make_body ctx x =
  ctx.mc_make_body x
