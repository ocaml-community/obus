(*
 * test_communication.ml
 * ---------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Test the communication with a message bus *)

open Lwt
open Lwt_io
open OBus_message

(* number of message to generate *)
let test_count = 100

let name = "obus.test.communication"

let rec run_tests con = function
  | 0 ->
      return ()
  | n ->
      let message = Gen_random.message () in
      let%lwt () = OBus_connection.send_message con {
        message with
          destination = name;
          typ = Signal(["obus"; "test"], "obus.test", "test");
      } in
      run_tests con (n - 1)

let rec wait_for_name con =
  OBus_bus.name_has_owner con name >>= function
    | true -> return ()
    | false -> let%lwt () = Lwt_unix.sleep 0.1 in wait_for_name con

let test () =
  let%lwt () = Lwt_io.flush Lwt_io.stdout in
  match Unix.fork () with
    | 0 ->
        let%lwt con = OBus_bus.session () in
        let%lwt () = wait_for_name con in
        let%lwt () = run_tests con test_count in
        exit 0
    | pid ->
        let%lwt () = printlf "sending and receiving %d messages through the message bus." test_count in
        let%lwt bus = OBus_bus.session () in
        let%lwt _ = OBus_bus.request_name bus name in
        let%lwt progress = Progress.make "received" test_count in
        let waiter, wakener = wait () in
        let count = ref 0 in
        ignore (Lwt_sequence.add_r
                  (function
                     | {  typ = Signal(["obus"; "test"], "obus.test", "test") } ->
                         ignore (Progress.incr progress);
                         incr count;
                         if !count = test_count then
                           wakeup wakener true;
                         None
                     | msg ->
                         Some msg)
                  (OBus_connection.incoming_filters bus));
        let%lwt result = waiter in
        let%lwt () = Progress.close progress in
        let%lwt _ = Lwt_unix.waitpid [] pid in
        return result
