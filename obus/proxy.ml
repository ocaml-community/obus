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
  name : name option;
  path : path;
}

let make connection interface name path =
  { connection = connection;
    interface = interface;
    name = name;
    path = path }

let path { path = x } = x
let name { name = x } = x
