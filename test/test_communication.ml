(*
 * test_communication.ml
 * ---------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Test the communication with a message bus *)

open Lwt
open Printf
open OBus_message

(* number of message to generate *)
let test_count = 100

let print_progress n =
  if n = 0 then
    printf " 0%!"
  else if n = test_count then
    printf "..%d\n%!" n
  else if n mod 10 = 0 then
    printf "..%d%!" n

let name = "obus.test.communication"

let rec run_tests con = function
  | 0 ->
      return ()
  | n ->
      OBus_connection.send_message con
        { Gen_random.message () with destination = Some name }
      >>= fun _ -> run_tests con (n - 1)

let rec wait_for_name con =
  OBus_bus.name_has_owner con name >>= function
    | true -> return ()
    | false -> Lwt_unix.sleep 0.1 >>= fun _ -> wait_for_name con

let _ =
  printf "sending and receiving %d messages through the message bus.\n%!" test_count;
  if Unix.fork () = 0 then
    Lwt_unix.run
      (perform
         con <-- Lazy.force OBus_bus.session >>= (fun bus -> return bus);
         wait_for_name con;
         run_tests con test_count;
         OBus_connection.demit_signal con ~destination:name ~interface:"a.a" ~member:"exit_now" ~path:[] [])
  else
    Lwt_unix.run
      (perform
         bus <-- Lazy.force OBus_bus.session;
         OBus_bus.request_name bus name;
         let received = ref 0
         and _ = printf "received: 0%!" in
         let _ = OBus_connection.add_incoming_filter bus
           (function
              | { typ = Signal(_, _, "exit_now") } ->
                  printf "\nexit signal received.\n%!";
                  OBus_connection.close bus;
                  None
              | { destination = Some n } when n = name ->
                  incr received;
                  print_progress !received;
                  None
              | msg -> Some msg) in
         OBus_connection.watch bus)
