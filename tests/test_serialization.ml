(*
 * test_serialization.ml
 * ---------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Testing of serialization/deserialization *)

open Lwt
open Lwt_io

(* number of message to generate *)
let test_count = 100

type result = {
  success : int;
  (* Writing/reading succeed and original and resulting messages are equal *)
  failure : int;
  (* Writing/reading succeed but original and resulting messages are not equal *)
  reading_error : int;
  (* Failed to deserialize the message *)
  writing_error : int;
  (* Falied to serialize the message *)
}

let run_one_test byte_order msg acc =
  try
    let str, fds = OBus_wire.string_of_message ~byte_order msg in
    let msg' = OBus_wire.message_of_string str fds in
    if msg' = msg then
      { acc with success = acc.success + 1 }
    else begin
      { acc with failure = acc.failure + 1 }
    end
  with
    | OBus_wire.Data_error msg ->
        { acc with writing_error = acc.writing_error + 1 }
    | OBus_wire.Protocol_error msg ->
        { acc with reading_error = acc.reading_error + 1 }

let run_tests prefix byte_order l =
  let%lwt progress = Progress.make prefix test_count in
  let rec aux acc n = function
    | [] ->
        let%lwt () = Progress.close progress in
        return acc
    | msg :: l ->
        let%lwt () = Progress.incr progress in
        aux (run_one_test byte_order msg acc) (n + 1) l
  in
  aux { success = 0; failure = 0; reading_error = 0; writing_error = 0 } 0 l

let print_result result =
  let%lwt () = printf "     success: %d\n" result.success in
  let%lwt () = printf "     failure: %d\n" result.failure in
  let%lwt () = printf "     writing error: %d\n" result.writing_error in
  let%lwt () = printf "     reading error: %d\n" result.reading_error in
  return ()

let rec gen_messages progress acc = function
  | 0 ->
      let%lwt () = Progress.close progress in
      return acc
  | n ->
      let%lwt () = Progress.incr progress in
      gen_messages progress (Gen_random.message () :: acc) (n - 1)

let test () =
  let%lwt progress = Progress.make (Printf.sprintf "generating %d messages" test_count) test_count in
  let%lwt msgs = gen_messages progress [] test_count in
  let%lwt () = printl "try to serialize/deserialize all messages and compare the result to the original message." in
  let%lwt result_le = run_tests "  - in little endian" Lwt_io.Little_endian msgs in
  let%lwt () = print_result result_le in
  let%lwt result_be = run_tests "  - in big endian" Lwt_io.Big_endian msgs in
  let%lwt () = print_result result_be in
  return (result_le.failure = 0
      && result_le.reading_error = 0
      && result_le.writing_error = 0
      && result_be.failure = 0
      && result_be.reading_error = 0
      && result_be.writing_error = 0)
