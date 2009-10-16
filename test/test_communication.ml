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
      lwt () = OBus_connection.send_message con { Gen_random.message () with destination = Some name } in
      run_tests con (n - 1)

let rec wait_for_name con =
  OBus_bus.name_has_owner con name >>= function
    | true -> return ()
    | false -> lwt () = Lwt_unix.sleep 0.1 in wait_for_name con

let () = Lwt_main.run (
  lwt () = printlf "sending and receiving %d messages through the message bus." test_count in
  if Unix.fork () = 0 then
    lwt con = Lazy.force OBus_bus.session in
    lwt () = wait_for_name con in
    lwt () = run_tests con test_count in
    OBus_connection.dyn_emit_signal con ~destination:name ~interface:"a.a" ~member:"exit_now" ~path:[] []
  else
    lwt bus = Lazy.force OBus_bus.session in
    lwt _ = OBus_bus.request_name bus name in
    let progress = Progress.make "received" test_count and waiter, wakener = wait () in
    ignore (Lwt_sequence.add_r
              (function
                 | { typ = Signal(_, _, "exit_now") } ->
                     Progress.close progress;
                     ignore_result (printf "\nexit signal received.\n");
                     wakeup wakener ();
                     None
                 | { destination = Some n } when n = name ->
                     Progress.incr progress;
                     None
                 | msg -> Some msg)
              (OBus_connection.incoming_filters bus));
    waiter
)
