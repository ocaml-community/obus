(*
 * proxy.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)


type name = string
type path = string

type 'a t = {
  connection : Connection.t;
  interface : 'a Interface.t;
  sender : name option;
  name : name option;
  path : path;
}

let make connection interface ?sender ?destination path =
  { connection = connection;
    interface = interface;
    name = destination;
    path = path;
    sender = sender }

let path { path = x } = x
let name { name = x } = x
let sender { sender = x } = x
let connection { connection = x } = x
