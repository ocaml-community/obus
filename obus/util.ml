(*
 * util.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

include Common_util
open ThreadImplem

let with_mutex m f =
  try
    Mutex.lock m;
    let result = f () in
      Mutex.unlock m;
      result
  with
      e ->
        Mutex.unlock m;
        raise e
