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
      lwt () = OBus_connection.send_message con {
        Gen_random.message () with
          destination = Some name;
          typ = Signal(["obus"; "test"], "obus.test", "test");
      } in
      run_tests con (n - 1)

let rec wait_for_name con =
  OBus_bus.name_has_owner con name >>= function
    | true -> return ()
    | false -> lwt () = Lwt_unix.sleep 0.1 in wait_for_name con

let test () =
  lwt () = Lwt_io.flush Lwt_io.stdout in
  match Unix.fork () with
    | 0 ->
        lwt con = OBus_bus.session () in
        lwt () = wait_for_name con in
        lwt () = run_tests con test_count in
        exit 0
    | pid ->
        lwt () = printlf "sending and receiving %d messages through the message bus." test_count in
        lwt bus = OBus_bus.session () in
        lwt _ = OBus_bus.request_name bus name in
        lwt progress = Progress.make "received" test_count in
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
        lwt result = waiter in
        lwt () = Progress.close progress in
        lwt _ = Lwt_unix.waitpid [] pid in
        return result
