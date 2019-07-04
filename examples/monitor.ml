(*
 * monitor.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* This sample illustrate the use of threads in D-Bus + use of
   filters. Filters are part of the lowlevel api. *)

open Lwt
open OBus_bus
open OBus_message
open OBus_value

let filter what_bus message =
  Format.printf "@[<hv 2>message intercepted on %s bus:@\n%a@]@." what_bus OBus_message.print message;
  (* Drop the message so we do not respond to method call *)
  None

let add_filter what_bus get_bus =
  let%lwt bus = get_bus () in
  let _ = Lwt_sequence.add_r (filter what_bus) (OBus_connection.incoming_filters bus) in
  Lwt_list.iter_p
    (fun typ -> OBus_bus.add_match bus (OBus_match.rule ~typ ()))
    [ `Method_call; `Method_return; `Error; `Signal ]

let () = Lwt_main.run begin
  let%lwt () = add_filter "session" OBus_bus.session <&> add_filter "system" OBus_bus.system in
  let%lwt () = Lwt_io.printlf "type Ctrl+C to stop" in
  fst (wait ())
end
