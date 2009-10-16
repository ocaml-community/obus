(*
 * test_serialization.ml
 * ---------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Testing of serialization/deserialization *)

open Printf

(* number of message to generate *)
let test_count = 100

let save_dir = Filename.concat Filename.temp_dir_name "obus-test-serialization"
let make_save_dir = lazy(if not (Sys.file_exists save_dir) then Unix.mkdir save_dir 0o755)

let save_message_to_file fname msg =
  Lazy.force make_save_dir;
  let oc = open_out (Filename.concat save_dir fname) in
  let pp = Format.formatter_of_out_channel oc in
  OBus_message.print pp msg;
  Format.pp_print_flush pp ();
  close_out oc

let num = ref 0
let save msg1 msg2 =
  save_message_to_file (sprintf "%03d-a" !num) msg1;
  save_message_to_file (sprintf "%03d-b" !num) msg2;
  incr num

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
    let msg' = OBus_wire.message_of_string (OBus_wire.string_of_message ~byte_order msg) in
    if msg' = msg then
      { acc with success = acc.success + 1 }
    else begin
      save msg msg';
      { acc with failure = acc.failure + 1 }
    end
  with
    | OBus_wire.Data_error msg ->
        { acc with writing_error = acc.writing_error + 1 }
    | OBus_wire.Protocol_error msg ->
        { acc with reading_error = acc.reading_error + 1 }

let run_tests prefix byte_order l =
  let progress = Progress.make prefix test_count in
  let rec aux acc n = function
    | [] ->
        Progress.close progress;
        acc
    | msg :: l ->
        Progress.incr progress;
        aux (run_one_test byte_order msg acc) (n + 1) l
  in
  aux { success = 0; failure = 0; reading_error = 0; writing_error = 0 } 0 l

let print_result result =
  printf "     success: %d\n" result.success;
  printf "     failure: %d\n" result.failure;
  printf "     writing error: %d\n" result.writing_error;
  printf "     reading error: %d\n" result.reading_error

let rec gen_messages progress acc = function
  | 0 ->
      Progress.close progress;
      acc
  | n ->
      Progress.incr progress;
      gen_messages progress (Gen_random.message () :: acc) (n - 1)

let _ =
  let msgs = gen_messages (Progress.make (sprintf "generating %d messages" test_count) test_count) [] test_count in
  printf "try to serialize/deserialize all messages and compare the result to the original message.\n";
  print_result (run_tests "  - in little endian" OBus_wire.Little_endian msgs);
  print_result (run_tests "  - in big endian" OBus_wire.Big_endian msgs);
  printf "failing tests have been saved in %s\n%!" save_dir
