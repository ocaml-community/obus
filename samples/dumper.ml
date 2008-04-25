(*
 * dumper.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus.Transport

let make prefix t = {
  backend = t.backend;
  recv = begin let n = ref 0 in
    fun buf pos count ->
      let len = t.recv buf pos count in
      let f = open_out (Printf.sprintf "%s%03d.recv" prefix !n) in
        incr n;
        output f buf pos len;
        close_out f;
        len
  end;
  send = begin let n = ref 0 in
    fun buf pos count ->
      let len = t.send buf pos count in
      let f = open_out (Printf.sprintf "%s%03d.send" prefix !n) in
        incr n;
        output f buf pos len;
        close_out f;
        len
  end;
  close = t.close;
}
