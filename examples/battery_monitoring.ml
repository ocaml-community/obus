(*
 * battery_monitoring.ml
 * ---------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt
open Lwt_io

(* List of monitored batteries. *)
let batteries = ref []

let print_state device state =
  printlf "state of %s: %s"
    (OBus_path.to_string (OBus_proxy.path (UPower_device.to_proxy device)))
    (match state with
       | `Unknown -> "unknown"
       | `Charging -> "charging"
       | `Discharging -> "discharging"
       | `Empty -> "empty"
       | `Fully_charged -> "fully charged"
       | `Pending_charge -> "pending charge"
       | `Pending_discharge -> "pending discharge")

(* Handle device addition. *)
let monitor_device device =
  if List.exists (fun (device', _, _) -> device = device') !batteries then
    return ()
  else begin
    let switch = Lwt_switch.create () in
    lwt signal = OBus_property.monitor (UPower_device.state device) in
    let id = Lwt_signal.notify_s (print_state device) signal in
    batteries := (device, switch, id) :: !batteries;
    return ()
  end

(* Handle device removal. *)
let unmonitor_device device =
  lwt () =
    Lwt_list.iter_p
      (fun (device', switch, id) ->
         if device = device' then begin
           Lwt_signal.disable id;
           Lwt_switch.turn_off switch
         end else
           return ())
      !batteries
  in
  batteries := List.filter (fun (device', _, _) -> device <> device') !batteries;
  return ()

lwt () =
  (* Get the manager proxy. *)
  lwt manager = UPower.daemon () in

  (* Handle device addition/removal. *)
  lwt () =
    OBus_signal.connect (UPower.device_added manager)
    >|= Lwt_event.always_notify_p monitor_device
  and () =
    OBus_signal.connect (UPower.device_removed manager)
    >|= Lwt_event.always_notify_p unmonitor_device
  in

  (* Monitor all the batteries initially present on the system. *)
  lwt devices = UPower.enumerate_devices manager in
  lwt () = Lwt_list.iter_p monitor_device devices in

  fst (wait ())
