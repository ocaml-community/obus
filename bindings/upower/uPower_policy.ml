(*
 * uPower_policy.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

type cookie = int

type latency = [ `Cpu_dma | `Network ]

let string_of_latency = function
  | `Cpu_dma -> "cpu_dma"
  | `Network -> "network"

let latency_of_string = function
  | "cpu_dma" -> `Cpu_dma
  | "network" -> `Network
  | latency -> Printf.ksprintf failwith "unknown latency type (%S)" latency

type latency_request = {
  lr_cookie : cookie;
  lr_uid : int;
  lr_pid : int;
  lr_exec : string;
  lr_timespec : int64;
  lr_persistent : bool;
  lr_typ : latency;
  lr_reserved : string;
  lr_value : int;
}

open UPower_interfaces.Org_freedesktop_UPower_QoS

let proxy daemon = OBus_proxy.make (UPower.to_peer daemon) ["org"; "freedesktop"; "UPower"; "Policy"]

let set_minimum_latency daemon ~latency ~value =
  OBus_method.call m_SetMinimumLatency (proxy daemon) (string_of_latency latency, Int32.of_int value)

let request_latency daemon ~latency ~value ~persistent =
  let value = Int32.of_int value in
  let%lwt cookie = OBus_method.call m_RequestLatency (proxy daemon) (string_of_latency latency, value, persistent) in
  let cookie = Int32.to_int cookie in
  return cookie

let cancel_request daemon ~latency ~cookie =
  let cookie = Int32.of_int cookie in
  OBus_method.call m_CancelRequest (proxy daemon) (string_of_latency latency, cookie)

let get_latency daemon ~latency =
  let%lwt value = OBus_method.call m_GetLatency (proxy daemon) (string_of_latency latency) in
  let value = Int32.to_int value in
  return value

let latency_changed daemon =
  OBus_signal.map
    (fun (latency, value) ->
       (latency_of_string latency, Int32.to_int value))
    (OBus_signal.make s_LatencyChanged (proxy daemon))

let get_latency_requests daemon =
  let%lwt requests = OBus_method.call m_GetLatencyRequests (proxy daemon) () in
  return
    (List.map
       (fun (cookie, uid, pid, exec, timespec, persistent, typ, reserved, value) -> {
          lr_cookie = Int32.to_int cookie;
          lr_uid = Int32.to_int uid;
          lr_pid = Int32.to_int pid;
          lr_exec = exec;
          lr_timespec = timespec;
          lr_persistent = persistent;
          lr_typ = latency_of_string typ;
          lr_reserved = reserved;
          lr_value = Int32.to_int value;
        })
       requests)

let requests_changed daemon =
  OBus_signal.make s_RequestsChanged (proxy daemon)
