(*
 * dyn.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_types
open OBus_dynamic

let v = make (tarray (tbasic tint32)) [1l; 2l; 3l]

let _ =
  let module M = Matcher(struct type ('a, 'cl) t = 'a -> int end) in
    Printf.printf "v : %s = %s\n" (OBus_types.string_of_single (typ v)) (string_of_single v);
    Printf.printf "%d\n"
      (with_single
         (object(self)
            inherit M.matcher
            method array t l = List.length l
          end) v)
