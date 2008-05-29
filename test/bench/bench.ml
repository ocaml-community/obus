(*
 * bench.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Printf
open OBus.Wire

let buffer = String.make 1000 '\x00'

module Check(W : Writer)(R : Reader) =
struct
  let check_one td tc f g x =
    f buffer 0 x;
    printf "  %s <-> %s : %s\n" td tc (string_of_bool (g buffer 0 = x))

  let check_all () =
    check_one "int" "int32" W.int_int32 R.int_int32 (-42);
    check_one "int" "uint32" W.int_uint32 R.int_uint32 0x12345678;
    check_one "int32" "int32" W.int32_int32 R.int32_int32 0x12345678l;
    check_one "int64" "int64" W.int64_int64 R.int64_int64 0x9012345678abcdefL;
end

module LECheck = Check(LEWriter)(LEReader)
module BECheck = Check(BEWriter)(BEReader)

let count = 10000000

module Perf(W : Writer)(R : Reader) =
struct
  let test_one oper td tc f =
    let t1 = Sys.time () in
      for i = 0 to count - 1 do
        ignore (f ());
      done;
      let t2 = Sys.time () in
        printf "time for %d %s of %s <-> %s : %f\n%!" count oper td tc (t2 -. t1)

  let testr td tc f =
    test_one "reading" td tc (fun () -> f buffer 0)

  let testw td tc f x =
    test_one "writing" td tc (fun () -> f buffer 0)

  let test_all () =
    testr "int" "int32" R.int_int32;
    testr "int" "uint32" R.int_uint32;
    testr "int32" "int32" R.int32_int32;
    testr "int64" "int64" R.int64_int64;
    testw "int" "int32" W.int_int32 (-42);
    testw "int" "uint32" W.int_uint32 0x12345678;
    testw "int32" "int32" W.int32_int32 0x12345678l;
    testw "int64" "int64" W.int64_int64 0x9012345678abcdefL;
end

module LEPerf = Perf(LEWriter)(LEReader)
module BEPerf = Perf(BEWriter)(BEReader)

let _ =
  printf "-- verifying that the readen/written data are correct.\n";
  printf "in little endian:\n";
  LECheck.check_all ();
  printf "in big endian:\n";
  BECheck.check_all ();
  printf "-- performance test\n";
  printf "in little endian:\n";
  LEPerf.test_all ();
  printf "in big endian:\n";
  BEPerf.test_all ()

