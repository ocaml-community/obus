(*
 * test_printing.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Format

let test_count = 10

let rec run_tests = function
  | 0 -> ()
  | n ->
      let msg = Gen_random.message () in
      printf "@[<2>message %d:@\n%a@]@.@\n" (test_count - n + 1) OBus_message.print msg;
      run_tests (n - 1)

let _ =
  printf "pretty-printing %d messages.@.@\n" test_count;
  run_tests test_count
