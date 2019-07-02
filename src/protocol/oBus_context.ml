(*
 * oBus_context.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type t = {
  connection : OBus_connection.t;
  flags : OBus_message.flags;
  sender : OBus_peer.t;
  destination : OBus_peer.t;
  serial : OBus_message.serial;
}

let key = Lwt.new_key ()

let get () =
  match Lwt.get key with
    | Some ctx -> ctx
    | None -> failwith "OBus_context.get: not in a method call handler"

let make ~connection ~message = {
  connection = connection;
  flags = OBus_message.flags message;
  sender = OBus_peer.make connection (OBus_message.sender message);
  destination = OBus_peer.make connection (OBus_message.destination message);
  serial = OBus_message.serial message;
}

let connection ctx = ctx.connection
let flags ctx = ctx.flags
let serial ctx = ctx.serial
let sender ctx = ctx.sender
let destination ctx = ctx.destination
