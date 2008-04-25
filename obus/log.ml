(*
 * log.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let (verbose,
     authentification,
     transport) =
  try
    let s = Sys.getenv "OBUSLOG" in
      if String.contains s '*'
      then (true, true, true)
      else (true,
            String.contains s 'a',
            String.contains s 't')
  with
      Not_found -> (false, false, false)
