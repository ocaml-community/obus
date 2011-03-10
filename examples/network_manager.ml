(*
 * network_manager.ml
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* This example illustrate the use of OBus to detect network-manager
   connections. *)

open React
open Lwt
open Lwt_io

(* Add an element to a list if it is not already part of it. *)
let add_to_list x l =
  if List.mem x l then l else x :: l

(* Map a signal with a function returning a thread. *)
let map_signal ?eq f s =
  lwt v = f (S.value s) in
  return (S.hold ?eq v (Lwt_event.map_s f (S.changes s)))

(* Transforms a signal of signals into a signal. *)
let flatten_signals s =
  S.switch (S.value s) (S.changes s)

lwt () =
  (* Get the manager. *)
  lwt manager = Nm_manager.daemon () in

  (* Create a signal holding the list of DHCP configurations with
     their options. *)
  lwt configs_with_options =
    OBus_property.monitor (Nm_manager.active_connections manager)
    >>= map_signal ~eq:(==) (Lwt_list.map_p (fun connection -> OBus_property.monitor (Nm_connection.devices connection)))
    >|= S.map ~eq:(==) (S.merge (List.fold_left (fun l device -> add_to_list device l)) [])
    >|= flatten_signals
    >>= map_signal ~eq:(==) (Lwt_list.map_p (fun device -> OBus_property.monitor (Nm_device.dhcp4_config device)))
    >|= S.map ~eq:(==) (S.merge (fun l config -> add_to_list config l) [])
    >|= flatten_signals
    >>= map_signal ~eq:(==) (Lwt_list.map_p (fun config -> OBus_property.monitor (Nm_dhcp4_config.options config) >|= S.map (fun options -> (config, options))))
    >|= S.map ~eq:(==) (S.merge (fun l x -> x :: l) [])
    >|= flatten_signals
  in

  (* Prints all configurations with their options when the list
     changes. *)
  Lwt_signal.always_notify_s
    (fun l ->
       lwt () = printl "DHCP options:" in
       Lwt_list.iter_s
         (fun (config, options) ->
            lwt () = printlf "  for %s:" (OBus_path.to_string (OBus_proxy.path (Nm_dhcp4_config.to_proxy config))) in
            Lwt_list.iter_s
              (fun (key, value) ->
                 printlf "    %s = %s" key (OBus_value.V.string_of_single value))
              options)
         l)
    configs_with_options;

  fst (wait ())
